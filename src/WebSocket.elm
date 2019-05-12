port module WebSocket exposing (Msg, State, init, listen, send, update)

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


listen : String -> (String -> msg) -> (Msg -> msg) -> Sub msg
listen url lmsg wrapper =
    let
        fn pmsg =
            if pmsg.tag == url then
                case Decode.decodeValue Decode.string pmsg.payload of
                    Ok str ->
                        lmsg str

                    Err err ->
                        Debug.todo <| Decode.errorToString err

            else
                wrapper <| toMsg pmsg.tag pmsg.payload
    in
    fromJs fn


send : String -> String -> State -> ( State, Cmd msg )
send url message (State model) =
    case Dict.get url model.sockets of
        Just (Connected socket) ->
            ( State model
            , mkSend socket url message
            )

        Just (Opening _) ->
            ( State { model | queues = Dict.insert url [ message ] model.queues }, Cmd.none )

        Nothing ->
            ( State { model | queues = Dict.insert url [ message ] model.queues }
            , toJs { tag = "open", payload = Encode.string url }
            )



--


type State
    = State Model


init =
    State
        { sockets = Dict.empty
        , queues = Dict.empty
        }


type alias Model =
    { sockets : SocketsDict
    , queues : QueuesDict
    }


type alias SocketsDict =
    Dict String Connection


type
    Connection
    --    = Opening Int Process.Id -- backoff, backoffID
    = Opening Int
    | Connected WS.WebSocket


type alias QueuesDict =
    Dict String (List String)



-- UPDATE


type Msg
    = Receive String Value
    | Open String -- url
    | Die String
    | GoodOpen String WS.WebSocket
    | BadOpen String
    | ConfirmSend String -- url
    | WSError String


update : Msg -> State -> ( State, Cmd Msg )
update msg (State model) =
    update_ msg model |> Tuple.mapFirst State


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    case msg of
        GoodOpen url socket ->
            ( { model | sockets = Dict.insert url (Connected socket) model.sockets }
            , model.queues
                |> getNextForUrl url
                |> Maybe.map (mkSend socket url)
                |> Maybe.withDefault Cmd.none
            )

        Open url ->
            ( { model | sockets = Dict.insert url (Opening 0) model.sockets }, Cmd.none )

        _ ->
            ( model, Cmd.none )


getNextForUrl url queues =
    Dict.get url queues |> Maybe.andThen List.head


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
    case tag of
        "ConfirmSend" ->
            case Decode.decodeValue decodeUrl payload of
                Ok url ->
                    ConfirmSend url

                Err err ->
                    WSError <| Decode.errorToString err

        "ConfirmOpen" ->
            case Decode.decodeValue decodeOpenSuccess payload of
                Ok ( url, socket ) ->
                    GoodOpen url socket

                Err err ->
                    WSError <| Decode.errorToString err

        _ ->
            Debug.todo tag


decodeOpenSuccess : Decoder ( String, WS.WebSocket )
decodeOpenSuccess =
    Decode.map2 Tuple.pair
        decodeUrl
        (Decode.field "socket" Decode.value)


decodeUrl =
    Decode.field "url" Decode.string
