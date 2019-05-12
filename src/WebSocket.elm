port module WebSocket exposing (Msg, State, init, listen, send, setSockets, update)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Process
import WebSocket.LowLevel as WS


type alias PortMsg =
    { tag : String
    , payload : Value
    }


port toJs : PortMsg -> Cmd msg


port fromJs : (PortMsg -> msg) -> Sub msg



--


listen : (Msg -> msg) -> State msg -> Sub msg
listen wrapper (State { sockets }) =
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
            , mkSend socket url message
            )

        Just (Opening _ _) ->
            ( State { model | queues = Dict.insert url [ message ] model.queues }, Cmd.none )

        Nothing ->
            ( State { model | queues = Dict.insert url [ message ] model.queues }
            , toJs { tag = "open", payload = Encode.string url }
            )



--


type State msg
    = State (Model msg)


init =
    State
        { sockets = Dict.empty
        , queues = Dict.empty
        }


setSockets : List ( String, String -> msg ) -> State msg -> ( State msg, Cmd Msg )
setSockets urls (State model) =
    let
        handleUrl ( url, constructor ) ( accSockets, accCmds ) =
            case Dict.get url accSockets of
                Just _ ->
                    ( accSockets, accCmds )

                Nothing ->
                    ( Dict.insert url (Opening constructor 0) accSockets, toJs { tag = "open", payload = Encode.string url } :: accCmds )

        ( sockets, addCmds ) =
            List.foldl handleUrl ( model.sockets, [] ) urls

        closeCmds =
            let
                go _ v acc =
                    case v of
                        Connected _ socket ->
                            toJs { tag = "close", payload = Encode.object [ ( "socket", socket ) ] } :: acc

                        _ ->
                            acc
            in
            List.foldl (\( k, _ ) acc -> Dict.remove k acc) model.sockets urls
                |> Dict.foldl go []
    in
    ( State { model | sockets = sockets }, Cmd.batch <| closeCmds ++ addCmds )


type alias Model msg =
    { sockets : SocketsDict msg
    , queues : QueuesDict
    }


type alias SocketsDict msg =
    Dict String (Connection msg)


type Connection msg
    = Opening (String -> msg) Int
      --    = Opening Int Process.Id -- backoff, backoffID
    | Connected (String -> msg) WS.WebSocket


type alias QueuesDict =
    Dict String (List String)



-- UPDATE


type Msg
    = GoodOpen ( String, WS.WebSocket )
    | ConfirmSend String -- url
    | BadOpen String
    | SocketClose CloseConfirmation
    | WSError String


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
                        |> Maybe.map (mkSend socket url)
                        |> Maybe.withDefault Cmd.none
                    )

                _ ->
                    Debug.todo "GoodOpen"

        BadOpen val ->
            let
                _ =
                    Debug.log "BadOpen" val
            in
            ( model, Cmd.none )

        SocketClose { url } ->
            ( { model | sockets = Dict.remove url model.sockets }, Cmd.none )

        ConfirmSend url ->
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
                |> Maybe.map (mkSendCmd newModel.sockets url)
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


mkSendCmd : SocketsDict msg -> String -> String -> Cmd Msg
mkSendCmd sockets url message =
    case Dict.get url sockets of
        Just (Connected _ socket) ->
            mkSend socket url message

        _ ->
            Cmd.none


mkSend : WS.WebSocket -> String -> String -> Cmd msg
mkSend socket url message =
    toJs
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
                |> recover (Decode.errorToString >> WSError)
    in
    case tag of
        "GoodOpen" ->
            handler decodeOpenSuccess GoodOpen

        "BadOpen" ->
            handler Decode.string BadOpen

        "ConfirmSend" ->
            handler decodeUrl ConfirmSend

        "SocketClose" ->
            handler decodeClose SocketClose

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
        (Decode.field "url" Decode.string)
        (Decode.field "code" Decode.int)
        (Decode.field "reason" Decode.string)
        (Decode.field "wasClean" Decode.bool)


decodeOpenSuccess : Decoder ( String, WS.WebSocket )
decodeOpenSuccess =
    Decode.map2 Tuple.pair
        decodeUrl
        (Decode.field "socket" Decode.value)


decodeUrl =
    Decode.field "url" Decode.string


recover : (a -> b) -> Result a b -> b
recover fn res =
    case res of
        Ok b ->
            b

        Err a ->
            fn a
