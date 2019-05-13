module WebSocket exposing (Msg, PortMsg, State, init, listen, send, setSockets, update)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Process
import Task


type State msg
    = State (Model msg)


type alias Model msg =
    { sockets : SocketsDict msg
    , queues : QueuesDict
    , toJs : PortMsg -> Cmd Msg
    }


type alias SocketsDict msg =
    Dict String (Connection msg)


type Connection msg
    = Opening (String -> msg) Float
      --    = Opening Int Process.Id -- backoff, backoffID
    | Connected (String -> msg) WebSocket


type alias QueuesDict =
    Dict String (List String)


init toJs =
    State
        { sockets = Dict.empty
        , queues = Dict.empty
        , toJs = toJs
        }


type alias WebSocket =
    Decode.Value


type alias PortMsg =
    { tag : String
    , payload : Value
    }



--


listen : (Msg -> msg) -> State msg -> ((PortMsg -> msg) -> Sub msg) -> Sub msg
listen wrapper (State { sockets }) fromJs =
    let
        fn pmsg =
            case Dict.get pmsg.tag sockets of
                Just (Connected lmsg _) ->
                    case Decode.decodeValue Decode.string pmsg.payload of
                        Ok str ->
                            lmsg str

                        Err err ->
                            Debug.todo <| Decode.errorToString err

                Just other ->
                    Debug.todo <| Debug.toString other

                Nothing ->
                    wrapper <| toMsg pmsg.tag pmsg.payload
    in
    fromJs fn


send : String -> String -> State msg -> ( State msg, Cmd Msg )
send url message (State model) =
    case Dict.get url model.sockets of
        Just (Connected _ socket) ->
            ( State model
            , model.toJs <| mkSend socket url message
            )

        Just (Opening _ _) ->
            ( State { model | queues = Dict.insert url [ message ] model.queues }, Cmd.none )

        Nothing ->
            ( State { model | queues = Dict.insert url [ message ] model.queues }
            , tryOpen model url
            )


tryOpen model url =
    model.toJs { tag = "open", payload = Encode.string url }



--


setSockets : List ( String, String -> msg ) -> State msg -> ( State msg, Cmd Msg )
setSockets urls (State model) =
    let
        handleUrl ( url, constructor ) ( accSockets, accCmds ) =
            case Dict.get url accSockets of
                Just _ ->
                    ( accSockets, accCmds )

                Nothing ->
                    ( Dict.insert url (Opening constructor 0) accSockets
                    , model.toJs { tag = "open", payload = Encode.string url } :: accCmds
                    )

        ( sockets, addCmds ) =
            List.foldl handleUrl ( model.sockets, [] ) urls

        closeCmds =
            let
                go _ v acc =
                    case v of
                        Connected _ socket ->
                            model.toJs { tag = "close", payload = Encode.object [ ( "socket", socket ) ] } :: acc

                        _ ->
                            acc
            in
            List.foldl (\( k, _ ) acc -> Dict.remove k acc) model.sockets urls
                |> Dict.foldl go []
    in
    ( State { model | sockets = sockets }, Cmd.batch <| closeCmds ++ addCmds )



-- UPDATE


type Msg
    = GoodOpen ( String, WebSocket )
    | BadOpen ( String, String )
    | SocketError ( String, Int ) -- url, readyState
    | TryOpen String -- url
    | GoodSend String -- url
    | BadSend ( String, String )
    | SocketClose CloseConfirmation
    | DecodeError String


update : Msg -> State msg -> ( State msg, Cmd Msg )
update msg (State model) =
    update_ msg model |> Tuple.mapFirst State


update_ : Msg -> Model msg -> ( Model msg, Cmd Msg )
update_ msg model =
    case msg of
        GoodOpen ( url, socket ) ->
            case Dict.get url model.sockets of
                Just (Opening constructor _) ->
                    ( { model | sockets = Dict.insert url (Connected constructor socket) model.sockets }
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
                Just (Opening constructor backoff) ->
                    ( { model | sockets = Dict.insert url (Opening constructor <| 2 * backoff) model.sockets }
                    , Process.sleep backoff |> Task.perform (\_ -> TryOpen url)
                    )

                _ ->
                    let
                        _ =
                            Debug.log url error
                    in
                    ( model, Cmd.none )

        SocketClose { url } ->
            ( { model | sockets = Dict.remove url model.sockets }, Cmd.none )

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


getNextForUrl : String -> QueuesDict -> Maybe String
getNextForUrl url queues =
    Dict.get url queues |> Maybe.andThen List.head


mkSendCmd : SocketsDict msg -> String -> String -> Maybe PortMsg
mkSendCmd sockets url message =
    case Dict.get url sockets of
        Just (Connected _ socket) ->
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
