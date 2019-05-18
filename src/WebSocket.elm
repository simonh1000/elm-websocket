module WebSocket exposing (ConnectionStatus(..), Msg, PortMsg, State, getStatus, init, listen, send, setSockets, update)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Process
import Task


type alias Model msg =
    { sockets : SocketsDict msg
    , queues : QueuesDict
    , toJs : PortMsg -> Cmd Msg
    }


type alias SocketsDict msg =
    Dict String ( String -> msg, Connection )


type Connection
    = Opening Float --  backoff
      --    = Opening  Float Process.Id -- routingMsg, backoff, id (so that we can kill if user no longer wants this socket)
    | Connected WebSocket -- routingMsg, socket


type ConnectionStatus
    = StatusOpening Float
    | StatusConnected
    | NoSocket


type alias QueuesDict =
    Dict String (List String)


type alias WebSocket =
    Decode.Value



-- API


type State msg
    = State (Model msg)


type alias PortMsg =
    { tag : String
    , payload : Value
    }


init toJs =
    State
        { sockets = Dict.empty
        , queues = Dict.empty
        , toJs = toJs
        }


{-| Client changes the socket they want to use

    - close those that are no longer required (whether open, or opening)
    - open those that we do no have

-}
setSockets : List ( String, String -> msg ) -> State msg -> ( State msg, Cmd Msg )
setSockets selectedSockets (State model) =
    let
        handleUrl ( url, msgRouter ) ( accSockets, accCmds ) =
            case Dict.get url model.sockets of
                Just socket ->
                    -- copy across existing socket
                    ( Dict.insert url socket accSockets, accCmds )

                Nothing ->
                    -- add a new one
                    ( Dict.insert url ( msgRouter, Opening 0 ) accSockets
                    , model.toJs { tag = "open", payload = Encode.string url } :: accCmds
                    )

        ( sockets, addCmds ) =
            List.foldl handleUrl ( Dict.empty, [] ) selectedSockets

        closeCmds =
            let
                go _ v acc =
                    case v of
                        ( _, Connected socket ) ->
                            model.toJs { tag = "close", payload = Encode.object [ ( "socket", socket ) ] } :: acc

                        _ ->
                            acc
            in
            -- get list of all existing sockets that are no longer required
            List.foldl (\( k, _ ) acc -> Dict.remove k acc) model.sockets selectedSockets
                -- close each one
                |> Dict.foldl go []
    in
    ( State { model | sockets = sockets }, Cmd.batch <| closeCmds ++ addCmds )


send : String -> String -> State msg -> ( State msg, Cmd Msg )
send url message (State model) =
    case Dict.get url model.sockets of
        Just ( _, Connected socket ) ->
            ( State model
            , model.toJs <| mkSend socket url message
            )

        Just ( _, Opening _ ) ->
            ( State { model | queues = Dict.insert url [ message ] model.queues }, Cmd.none )

        Nothing ->
            -- there is no socket foreseen for url.
            -- We will queue the message pending reconnection
            ( State { model | queues = Dict.insert url [ message ] model.queues }, Cmd.none )


update : Msg -> State msg -> ( State msg, Cmd Msg )
update msg (State model) =
    update_ msg model |> Tuple.mapFirst State


getStatus url (State model) =
    case Dict.get url model.sockets of
        Just ( _, Connected _ ) ->
            StatusConnected

        Just ( _, Opening backoff ) ->
            StatusOpening backoff

        Nothing ->
            NoSocket



-- Subscriptions


{-|

    wrapper is the message to route to this library's update function
    the routing messages for each socket are stored in state.sockets

-}
listen : (Msg -> msg) -> State msg -> (PortMsg -> msg)
listen wrapper (State { sockets }) =
    \pmsg ->
        case Dict.get pmsg.tag sockets of
            Just ( msgRouter, Connected _ ) ->
                case Decode.decodeValue Decode.string pmsg.payload of
                    Ok str ->
                        -- deliver immediately to client
                        msgRouter str

                    Err err ->
                        wrapper <| BadMessage <| Decode.errorToString err

            Just other ->
                -- a message from a socket thought to be closed!
                Debug.todo <| Debug.toString other

            Nothing ->
                wrapper <| toMsg pmsg.tag pmsg.payload



-- UPDATE (private)


type Msg
    = GoodOpen ( String, WebSocket )
    | BadOpen ( String, String ) -- does not exist
    | SocketError ( String, Int ) -- url, readyState
    | SocketClose CloseConfirmation
    | TryOpen String -- url
    | GoodSend String -- url
    | BadSend ( String, String )
    | BadMessage String
    | DecodeError String


update_ : Msg -> Model msg -> ( Model msg, Cmd Msg )
update_ msg model =
    case msg of
        GoodOpen ( url, socket ) ->
            case Dict.get url model.sockets of
                Just ( msgRouter, Opening _ ) ->
                    ( { model | sockets = Dict.insert url ( msgRouter, Connected socket ) model.sockets }
                    , model.queues
                        |> getNextForUrl url
                        |> Maybe.map (mkSend socket url >> model.toJs)
                        |> Maybe.withDefault Cmd.none
                    )

                _ ->
                    Debug.todo "GoodOpen"

        --        BadOpen ( url, error ) ->
        --            case Dict.get url model.sockets of
        --                Just (Opening constructor backoff) ->
        --                    ( { model | sockets = Dict.insert url (Opening constructor <| 2 * backoff) model.sockets }
        --                    , Process.sleep backoff |> Task.perform (\_ -> TryOpen url)
        --                    )
        --
        --                _ ->
        --                    let
        --                        _ =
        --                            Debug.log url error
        --                    in
        --                    ( model, Cmd.none )
        TryOpen url ->
            ( model, tryOpen model url )

        SocketError ( url, error ) ->
            case Dict.get url model.sockets of
                Just ( msgRouter, Opening backoff ) ->
                    ( { model | sockets = Dict.insert url ( msgRouter, Opening (Debug.log "backoff" <| 1 + backoff) ) model.sockets }
                    , Process.sleep (2000 * backoff) |> Task.perform (\_ -> TryOpen url)
                    )

                _ ->
                    let
                        _ =
                            Debug.log url error
                    in
                    ( model, Cmd.none )

        SocketClose res ->
            let
                _ =
                    Debug.log "SocketClose" res
            in
            --            ( { model | sockets = Dict.remove res.url model.sockets }, Cmd.none )
            case Dict.get res.url model.sockets of
                Just ( msgRouter, _ ) ->
                    -- not clear what needs to be done hear as SocketError will have already fired?
                    --                    let
                    --                        m =
                    --                            { model | sockets = Dict.insert res.url ( msgRouter, Opening 0 ) model.sockets }
                    --                    in
                    --                    ( m, tryOpen m res.url )
                    ( model, Cmd.none )

                Nothing ->
                    -- user no longer wants this socket (they probably shut it themselves in fact)
                    ( model, Cmd.none )

        GoodSend url ->
            let
                newModel =
                    case Dict.get url model.queues of
                        Just (_ :: tl) ->
                            { model | queues = Dict.insert url tl model.queues }

                        _ ->
                            model
            in
            ( newModel
            , newModel.queues
                |> getNextForUrl url
                |> Maybe.andThen (mkSendCmd newModel.sockets url)
                |> Maybe.map model.toJs
                |> Maybe.withDefault Cmd.none
            )

        _ ->
            let
                _ =
                    Debug.log "unhandled" msg
            in
            ( model, Cmd.none )



-- helpers


getNextForUrl : String -> QueuesDict -> Maybe String
getNextForUrl url queues =
    Dict.get url queues |> Maybe.andThen List.head


mkSendCmd : SocketsDict msg -> String -> String -> Maybe PortMsg
mkSendCmd sockets url message =
    case Dict.get url sockets of
        Just ( _, Connected socket ) ->
            Just <| mkSend socket url message

        _ ->
            Nothing


tryOpen model url =
    model.toJs { tag = "open", payload = Encode.string url }


mkSend : WebSocket -> String -> String -> PortMsg
mkSend socket url message =
    { tag = "send"
    , payload =
        Encode.object
            [ ( "socket", socket )
            , ( "url", Encode.string url )
            , ( "message", Encode.string message )
            ]
    }


toMsg tag payload =
    let
        handler dec msg =
            Decode.decodeValue dec payload
                |> Result.map msg
                |> recover (Decode.errorToString >> DecodeError)
    in
    case tag of
        "GoodOpen" ->
            handler decodeGoodOpen GoodOpen

        "BadOpen" ->
            handler decodeBadAction BadOpen

        "GoodSend" ->
            handler decodeUrl GoodSend

        "BadSend" ->
            handler decodeBadAction BadSend

        "close" ->
            handler decodeClose SocketClose

        "error" ->
            handler decodeError SocketError

        _ ->
            Debug.todo tag


type alias CloseConfirmation =
    { url : String
    , code : Int
    , reason : String
    , wasClean : Bool
    }


decodeClose =
    Decode.map4 CloseConfirmation
        decodeUrl
        (Decode.field "code" Decode.int)
        (Decode.field "reason" Decode.string)
        (Decode.field "wasClean" Decode.bool)


decodeGoodOpen : Decoder ( String, WebSocket )
decodeGoodOpen =
    Decode.map2 Tuple.pair
        decodeUrl
        (Decode.field "socket" Decode.value)


decodeBadAction : Decoder ( String, String )
decodeBadAction =
    Decode.map2 Tuple.pair decodeUrl (Decode.field "error" Decode.string)


decodeError : Decoder ( String, Int )
decodeError =
    Decode.map2 Tuple.pair decodeUrl (Decode.field "readyState" Decode.int)


decodeUrl =
    Decode.field "url" Decode.string


recover : (a -> b) -> Result a b -> b
recover fn res =
    case res of
        Ok b ->
            b

        Err a ->
            fn a
