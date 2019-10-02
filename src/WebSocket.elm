module WebSocket exposing (ConnectionStatus(..), Msg, PortMsg, State, WSConfig, closeSocket, getStatus, init, listen, send, setSockets, update)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Process
import Task



-- -------------------
-- PUBLIC API
-- -------------------


{-| Opaque type that you need to include in your model
-}
type State
    = State Model


{-| Create a new WebSocket State
-}
init =
    State initModel


{-| The type of the data structure you need to provide in/out ports for
-}
type alias PortMsg =
    { tag : String
    , -- must be Value as we passing sockets, as well as string data
      payload : Value
    }


type alias WSConfig msg =
    { -- a port
      toSocket : PortMsg -> msg
    , -- returns the url that closed
      onClosedDown : String -> msg
    , -- optional, user defined value (to help monitor closing)
      uid : Maybe String
    }


{-| Client changes the socket they want to use

    - close those that are no longer required (whether open, or opening)
    - open those that we do no have

-}
setSockets : WSConfig msg -> List String -> State -> ( State, Cmd Msg, List msg )
setSockets config urls (State model) =
    setSockets_ urls model |> convertFromPrivate config


closeSocket : WSConfig msg -> String -> State -> ( State, Cmd Msg, List msg )
closeSocket config url (State model) =
    closeSocket_ config.uid url model |> convertFromPrivate config


{-| -}
send : WSConfig msg -> String -> String -> State -> ( State, Cmd Msg, List msg )
send config url message (State model) =
    send_ url message model |> convertFromPrivate config


type Msg
    = GoodOpen ( String, WebSocket )
    | BadOpen ( String, String ) -- does not exist
    | SocketError ( String, Int ) -- url, readyState
    | SocketClose CloseConfirmation
    | TryOpen String -- url (callback from sleeping for backoff period)
    | GoodSend String -- url
    | BadSend ( String, String )
    | DecodeError Decode.Error
    | NoOp


{-| -}
update : WSConfig msg -> Msg -> State -> ( State, Cmd Msg, List msg )
update config msg (State model) =
    update_ msg model |> convertFromPrivate config


{-| Defines the status of the connection as reported to users of the package
-}
type ConnectionStatus
    = SocketOpening Float
    | SocketConnected
    | NoSocket


{-| Returns the status of the socket
-}
getStatus : String -> State -> ConnectionStatus
getStatus url (State model) =
    case Dict.get url model.sockets of
        Just (Connected _) ->
            SocketConnected

        Just (Opening backoff) ->
            SocketOpening backoff

        Nothing ->
            NoSocket



-- Subscriptions


{-| Instead of listening to each message individually, we need to pass in all the options as a Dictionary
and as the WebSocket.Msg wrapper

    wrapper is the message to route to this library's update function
    the routing messages for each socket are stored in state.sockets

-}
listen : Dict String (String -> msg) -> (Msg -> msg) -> (PortMsg -> msg)
listen dict wsMsg =
    \pmsg ->
        case Dict.get pmsg.tag dict of
            Just fn ->
                -- we have a message from a socket
                Decode.decodeValue Decode.string pmsg.payload
                    |> Result.map fn
                    |> extract (wsMsg << DecodeError)

            Nothing ->
                wsMsg <| convertIncomingMsg pmsg



-- PRIVATE


type alias Model =
    { sockets : SocketsDict
    , queues : QueuesDict
    }


initModel : Model
initModel =
    { sockets = Dict.empty
    , queues = Dict.empty
    }


type alias SocketsDict =
    Dict String Connection


type Connection
    = Opening Float --  backoff
      --    = Opening  Float Process.Id -- backoff, id (so that we can kill if user no longer wants this socket)
    | Connected WebSocket -- routingMsg, socket


type alias QueuesDict =
    Dict String (List String)


type alias WebSocket =
    Decode.Value


convertFromPrivate : WSConfig msg -> ( Model, CmdMsg ) -> ( State, Cmd Msg, List msg )
convertFromPrivate config ( model, cmd ) =
    convertCmdMsg config model cmd
        |> (\( cmds, msgs ) -> ( State model, cmds, msgs ))


type CmdMsg
    = Delay Float Msg
    | SendToJs PortMsg
    | BatchMsg (List CmdMsg)
    | ReportError String
    | SocketClosedCleanly String -- url
    | CmdNone


convertCmdMsg : WSConfig msg -> Model -> CmdMsg -> ( Cmd Msg, List msg )
convertCmdMsg config model cmd =
    case cmd of
        Delay backoff msg ->
            ( Process.sleep (2000 * backoff)
                |> Task.andThen (\_ -> Task.succeed msg)
                |> Task.perform (\_ -> NoOp)
            , []
            )

        SendToJs pmsg ->
            ( Cmd.none, [ config.toSocket pmsg ] )

        SocketClosedCleanly url ->
            ( Cmd.none, [ config.onClosedDown url ] )

        BatchMsg lst ->
            let
                go item ( accCmds, accMgs ) =
                    convertCmdMsg config model item
                        |> Tuple.mapFirst (\cmd_ -> cmd_ :: accCmds)
                        |> Tuple.mapSecond (\msg -> msg ++ accMgs)
            in
            List.foldl go ( [], [] ) lst
                |> Tuple.mapFirst Cmd.batch

        ReportError _ ->
            ( Cmd.none, [] )

        CmdNone ->
            ( Cmd.none, [] )


update_ : Msg -> Model -> ( Model, CmdMsg )
update_ msg model =
    let
        addBackoff url backoff =
            ( { model | sockets = Dict.insert url (Opening <| 1 + backoff) model.sockets }
            , Delay backoff (TryOpen url)
            )
    in
    case msg of
        GoodOpen ( url, socket ) ->
            let
                newModel =
                    { model | sockets = Dict.insert url (Connected socket) model.sockets }
            in
            ( newModel, mkSendCmd newModel url |> Maybe.map SendToJs |> Maybe.withDefault CmdNone )

        BadOpen ( url, error ) ->
            case Dict.get url model.sockets of
                Just (Opening backoff) ->
                    addBackoff url backoff
                        |> (\( m, c ) -> ( m, BatchMsg [ c, ReportError <| "BadOpen" ++ error ] ))

                _ ->
                    -- ignore as user no longer cares about this socket
                    ( model, CmdNone )

        SocketError ( url, error ) ->
            ( model, ReportError <| "Error " ++ url ++ ": " ++ String.fromInt error )

        SocketClose { url } ->
            case Dict.get url model.sockets of
                Just (Opening backoff) ->
                    addBackoff url backoff

                Just (Connected _) ->
                    -- TODO add test for closing by user request rather than as a problem
                    -- reopen connection connection if closed down badly
                    --                    ( { model | sockets = Dict.insert url (Opening 0) model.sockets }
                    --                    , SendToJs <| mkOpenMsg url
                    --                    )
                    ( { model | sockets = Dict.remove url model.sockets }, SocketClosedCleanly url )

                Nothing ->
                    -- ignoring as user no longer cares about this socket
                    ( model, CmdNone )

        TryOpen url ->
            ( model, SendToJs <| mkOpenMsg url )

        GoodSend url ->
            let
                newModel =
                    -- head of queue is now known to have been sent Ok
                    { model | queues = Dict.update url (Maybe.andThen List.tail) model.queues }
            in
            ( newModel, mkSendCmd newModel url |> Maybe.map SendToJs |> Maybe.withDefault CmdNone )

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
                        ( m, SendToJs <| mkOpenMsg url )

                    else
                        ( model, ReportError <| "BadSend: " ++ reason )

                _ ->
                    -- ignore as user no longer cares about this socket
                    ( model, CmdNone )

        DecodeError err ->
            ( model, ReportError <| Decode.errorToString err )

        NoOp ->
            ( model, CmdNone )


setSockets_ : List String -> Model -> ( Model, CmdMsg )
setSockets_ urls model =
    let
        handleUrl url ( accSockets, accCmds ) =
            case Dict.get url model.sockets of
                Just socket ->
                    -- copy across existing socket
                    ( Dict.insert url socket accSockets, accCmds )

                Nothing ->
                    -- this is a new socket, so we will need to open it
                    ( Dict.insert url (Opening 0) accSockets
                    , SendToJs (mkOpenMsg url) :: accCmds
                    )

        ( sockets, addCmds ) =
            List.foldl handleUrl ( Dict.empty, [] ) urls

        closeCmds : List CmdMsg
        closeCmds =
            let
                go k v acc =
                    case v of
                        Connected socket ->
                            SendToJs (mkCloseMsg Nothing socket) :: acc

                        _ ->
                            -- ideally we want to stop the sleep process
                            acc
            in
            -- get list of all existing sockets that are no longer required
            List.foldl (\k acc -> Dict.remove k acc) model.sockets urls
                -- close each one
                |> Dict.foldl go []
    in
    ( { model | sockets = sockets }
    , BatchMsg (closeCmds ++ addCmds)
    )


closeSocket_ : Maybe String -> String -> Model -> ( Model, CmdMsg )
closeSocket_ mbUid url model =
    case Dict.get url model.sockets of
        Just (Connected socket) ->
            ( model, SendToJs <| mkCloseMsg mbUid socket )

        _ ->
            ( model, CmdNone )


send_ : String -> String -> Model -> ( Model, CmdMsg )
send_ url message model =
    let
        queueLength =
            model.queues |> Dict.get url |> Maybe.map List.length |> Maybe.withDefault 0

        newModel =
            addToQueue url message model
    in
    case Dict.get url model.sockets of
        Just (Connected socket) ->
            if queueLength == 0 then
                -- as there is nothing in the queue we will try to send immediately
                ( newModel
                , SendToJs <| mkSend socket url message
                )

            else
                -- i.e. add to end of queue
                ( newModel, CmdNone )

        Just (Opening _) ->
            -- still opening, add to end of queue
            ( newModel, CmdNone )

        Nothing ->
            -- there is no socket foreseen for url.
            -- We will queue the message pending reconnection
            ( newModel, CmdNone )



-- helpers (outgoing)


{-| Appends to existing queue or creates new one if needed
-}
addToQueue : String -> String -> Model -> Model
addToQueue url msg model =
    { model | queues = Dict.update url (\mbQueue -> Just <| (mbQueue |> Maybe.withDefault []) ++ [ msg ]) model.queues }


mkSendCmd : Model -> String -> Maybe PortMsg
mkSendCmd model url =
    case Dict.get url model.sockets of
        Just (Connected socket) ->
            model.queues
                |> getNextMsg url
                |> Maybe.map (mkSend socket url)

        _ ->
            Nothing


getNextMsg : String -> QueuesDict -> Maybe String
getNextMsg url queues =
    Dict.get url queues |> Maybe.andThen List.head


mkOpenMsg : String -> PortMsg
mkOpenMsg url =
    { tag = "open", payload = Encode.string url }


mkCloseMsg : Maybe String -> Value -> PortMsg
mkCloseMsg mbUid socket =
    let
        uidPart =
            case mbUid of
                Just uid ->
                    [ ( "uid", Encode.string uid ) ]

                Nothing ->
                    []
    in
    { tag = "close", payload = Encode.object <| ( "socket", socket ) :: uidPart }


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


convertIncomingMsg : PortMsg -> Msg
convertIncomingMsg { tag, payload } =
    let
        handler dec msg =
            Decode.decodeValue dec payload
                |> Result.map msg
                |> extract DecodeError
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
            handler (Decode.fail <| "convertIncomingMsg did not recognise " ++ tag) (\_ -> NoOp)


decodeGoodOpen : Decoder ( String, WebSocket )
decodeGoodOpen =
    Decode.map2 Tuple.pair
        decodeUrl
        (Decode.field "socket" Decode.value)


type alias CloseConfirmation =
    { url : String
    , code : Int
    , reason : String
    , wasClean : Bool
    }


decodeClose : Decoder CloseConfirmation
decodeClose =
    Decode.map4 CloseConfirmation
        decodeUrl
        (Decode.field "code" Decode.int)
        (Decode.field "reason" Decode.string)
        (Decode.field "wasClean" Decode.bool)


decodeBadAction : Decoder ( String, String )
decodeBadAction =
    Decode.map2 Tuple.pair decodeUrl (Decode.field "error" Decode.string)


decodeError : Decoder ( String, Int )
decodeError =
    Decode.map2 Tuple.pair decodeUrl (Decode.field "readyState" Decode.int)


decodeUrl : Decoder String
decodeUrl =
    Decode.field "url" Decode.string



-- == Result.Extra


extract : (a -> b) -> Result a b -> b
extract fn res =
    case res of
        Ok b ->
            b

        Err a ->
            fn a
