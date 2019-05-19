module WebSocket exposing (ConnectionStatus(..), Msg, PortMsg, State, getStatus, init, listen, send, setSockets, update)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Process
import Task


type alias Model =
    { sockets : SocketsDict
    , queues : QueuesDict
    , toJs : PortMsg -> Cmd Msg
    }


type alias SocketsDict =
    Dict String Connection



-- (subscription handler, connection)


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


type State
    = State Model


type alias PortMsg =
    { tag : String
    , -- must be value as we passing through a socket as well as string data
      payload : Value
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
setSockets : List String -> State -> ( State, Cmd Msg )
setSockets urls (State model) =
    let
        handleUrl url ( accSockets, accCmds ) =
            case Dict.get url model.sockets of
                Just socket ->
                    -- copy across existing socket
                    ( Dict.insert url socket accSockets, accCmds )

                Nothing ->
                    -- this is a new socket, so we will need to open it
                    ( Dict.insert url (Opening 0) accSockets
                    , { tag = "open", payload = Encode.string url } :: accCmds
                    )

        ( sockets, addCmds ) =
            List.foldl handleUrl ( Dict.empty, [] ) urls

        closeCmds =
            let
                go _ v acc =
                    case v of
                        Connected socket ->
                            { tag = "close", payload = Encode.object [ ( "socket", socket ) ] } :: acc

                        _ ->
                            acc
            in
            -- get list of all existing sockets that are no longer required
            List.foldl (\k acc -> Dict.remove k acc) model.sockets urls
                -- close each one
                |> Dict.foldl go []
    in
    ( State { model | sockets = sockets }
    , closeCmds ++ addCmds |> List.map model.toJs |> Cmd.batch
    )


send : String -> String -> State -> ( State, Cmd Msg )
send url message (State model) =
    let
        queueLength =
            model.queues |> Dict.get url |> Maybe.map List.length |> Maybe.withDefault 0

        newModel =
            State <| addToQueue url message model
    in
    case Dict.get url model.sockets of
        Just (Connected socket) ->
            if Debug.log "send" <| queueLength == 0 then
                -- as there is nothing in the queue we will try to send immediately
                ( newModel
                , model.toJs <| mkSend socket url message
                )

            else
                ( newModel, Cmd.none )

        Just (Opening _) ->
            ( newModel, Cmd.none )

        Nothing ->
            -- there is no socket foreseen for url.
            -- We will queue the message pending reconnection
            ( newModel, Cmd.none )


update : Msg -> State -> ( State, Cmd Msg )
update msg (State model) =
    update1 msg model |> Tuple.mapFirst State


getStatus : String -> State -> ConnectionStatus
getStatus url (State model) =
    case Dict.get url model.sockets of
        Just (Connected _) ->
            StatusConnected

        Just (Opening backoff) ->
            StatusOpening backoff

        Nothing ->
            NoSocket



-- Subscriptions


{-|

    wrapper is the message to route to this library's update function
    the routing messages for each socket are stored in state.sockets

-}



--listen : (Msg -> msg) ->State -> (PortMsg -> msg)
--listen wrapper (State { sockets }) =
--    \pmsg ->
--        case Dict.get pmsg.tag sockets of
--            Just ( msgRouter, Connected _ ) ->
--                case Debug.log "listen" <| Decode.decodeValue Decode.string pmsg.payload of
--                    Ok str ->
--                        -- deliver immediately to client
--                        msgRouter str
--
--                    Err err ->
--                        wrapper <| DecodeError <| Decode.errorToString err
--
--            Just other ->
--                -- a message from a socket thought to be closed!
--                Debug.todo <| Debug.toString other
--
--            Nothing ->
--                wrapper <| convertIncomingMsg pmsg.tag pmsg.payload


listen : Dict String (String -> msg) -> (Msg -> msg) -> (PortMsg -> msg)
listen dict wsMsg =
    \pmsg ->
        case Dict.get pmsg.tag dict of
            Just fn ->
                case Decode.decodeValue Decode.string pmsg.payload of
                    Ok str ->
                        fn str

                    Err err ->
                        wsMsg <| DecodeError err

            Nothing ->
                wsMsg <| convertIncomingMsg pmsg.tag pmsg.payload



-- UPDATE (private)


type Msg
    = GoodOpen ( String, WebSocket )
    | BadOpen ( String, String ) -- does not exist
    | SocketError ( String, Int ) -- url, readyState
    | SocketClose CloseConfirmation
    | TryOpen String -- url
    | GoodSend String -- url
    | BadSend ( String, String )
    | DecodeError Decode.Error


update1 : Msg -> Model -> ( Model, Cmd Msg )
update1 msg model =
    let
        addBackoff url backoff =
            ( { model | sockets = Dict.insert url (Opening (1 + backoff)) model.sockets }
            , Process.sleep (2000 * backoff) |> Task.perform (\_ -> TryOpen url)
            )
    in
    case msg of
        SocketClose { url } ->
            case Dict.get url model.sockets of
                Just (Opening backoff) ->
                    addBackoff url backoff

                Just (Connected _) ->
                    ( { model | sockets = Dict.insert url (Opening 0) model.sockets }
                    , model.toJs <| mkOpenMsg url
                    )

                Nothing ->
                    -- ignoring as user no longer cares about this socket
                    ( model, Cmd.none )

        BadSend ( url, reason ) ->
            case Dict.get url model.sockets of
                Just (Opening backoff) ->
                    addBackoff url backoff

                Just (Connected _) ->
                    if reason == "NotOpen" then
                        let
                            m =
                                { model | sockets = Dict.insert url (Opening 0) model.sockets }
                        in
                        ( m, model.toJs <| mkOpenMsg url )

                    else
                        let
                            _ =
                                Debug.log "BadSend" reason
                        in
                        ( model, Cmd.none )

                _ ->
                    -- ignore as user no longer cares about this socket
                    ( model, Cmd.none )

        BadOpen ( url, error ) ->
            case Dict.get url model.sockets of
                Just (Opening backoff) ->
                    addBackoff url backoff

                _ ->
                    let
                        _ =
                            Debug.log url error
                    in
                    ( model, Cmd.none )

        _ ->
            update2 msg model
                |> Tuple.mapSecond (Maybe.map model.toJs >> Maybe.withDefault Cmd.none)


update2 : Msg -> Model -> ( Model, Maybe PortMsg )
update2 msg model =
    case msg of
        GoodOpen ( url, socket ) ->
            case Dict.get url model.sockets of
                Just (Opening _) ->
                    ( { model | sockets = Dict.insert url (Connected socket) model.sockets }
                    , model.queues
                        |> getNextForUrl url
                        |> Maybe.map (mkSend socket url)
                    )

                _ ->
                    Debug.todo "GoodOpen"

        TryOpen url ->
            ( model, Just <| mkOpenMsg url )

        GoodSend url ->
            let
                newModel =
                    { model | queues = Dict.update url (Maybe.andThen List.tail) model.queues }
            in
            ( newModel
            , newModel.queues
                |> getNextForUrl url
                |> Maybe.andThen (mkSendCmd newModel.sockets url)
            )

        SocketError ( url, error ) ->
            let
                _ =
                    Debug.log ("Error " ++ url) error
            in
            ( model, Nothing )

        _ ->
            let
                _ =
                    Debug.log "unhandled" msg
            in
            ( model, Nothing )



-- helpers (outgoing)


mkOpenMsg : String -> PortMsg
mkOpenMsg url =
    { tag = "open", payload = Encode.string url }


addToQueue : String -> String -> Model -> Model
addToQueue url msg model =
    { model | queues = Dict.update url (\mbQueue -> Just <| (mbQueue |> Maybe.withDefault []) ++ [ msg ]) model.queues }


getNextForUrl : String -> QueuesDict -> Maybe String
getNextForUrl url queues =
    Dict.get url queues |> Maybe.andThen List.head


mkSendCmd : SocketsDict -> String -> String -> Maybe PortMsg
mkSendCmd sockets url message =
    case Dict.get url sockets of
        Just (Connected socket) ->
            Just <| mkSend socket url message

        _ ->
            Nothing


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



-- helpers (incoming)


convertIncomingMsg tag payload =
    let
        handler dec msg =
            Decode.decodeValue dec payload
                |> Result.map msg
                |> recover DecodeError
    in
    case tag of
        "GoodOpen" ->
            handler decodeGoodOpen GoodOpen

        "BadOpen" ->
            handler decodeBadAction BadOpen

        "GoodSend" ->
            handler decodeUrl GoodSend

        "BadSend" ->
            handler decodeBadSend BadSend

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


decodeBadSend : Decoder ( String, String )
decodeBadSend =
    Decode.map2 Tuple.pair decodeUrl (Decode.field "reason" Decode.string)


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
