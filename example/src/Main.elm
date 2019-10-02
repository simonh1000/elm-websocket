port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import WebSocket exposing (PortMsg)


url : String
url =
    "wss://echo.websocket.org"



-- ---------------------------
-- PORTS
-- ---------------------------


port fromSocket : (WebSocket.PortMsg -> msg) -> Sub msg


port toSocket : WebSocket.PortMsg -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { ws : WebSocket.State
    , message : String
    , count : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ws = WebSocket.init, message = "", count = 1 }, Cmd.none )


wsConfig : WebSocket.WSConfig Msg
wsConfig =
    { toSocket = OnMsgToSend
    , onClosedDown = OnClosedDown
    , uid = Just "myuid"
    }



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Send
    | Disconnect
    | Connect
    | WSMsg WebSocket.Msg
    | OnMsgToSend PortMsg
    | OnClosedDown String
    | OnEcho String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnEcho str ->
            ( { model | message = str }, Cmd.none )

        Connect ->
            WebSocket.setSockets wsConfig (Dict.keys listeners) model.ws
                |> handleSocket model

        Send ->
            WebSocket.send wsConfig url ("message: " ++ String.fromInt model.count) model.ws
                |> handleSocket { model | count = model.count + 1 }

        Disconnect ->
            WebSocket.closeSocket wsConfig url model.ws
                -- FIXME ths switching sockets does not return the uid
                -- WebSocket.setSockets wsConfig [] model.ws
                |> handleSocket model

        WSMsg msg ->
            WebSocket.update wsConfig msg model.ws
                |> handleSocket model

        OnMsgToSend portMsg ->
            ( model, toSocket portMsg )

        OnClosedDown uid ->
            let
                _ =
                    Debug.log "closed" uid
            in
            ( model, Cmd.none )


handleSocket : Model -> ( WebSocket.State, Cmd WebSocket.Msg, List Msg ) -> ( Model, Cmd Msg )
handleSocket model ( ws, socketCmd, msgs ) =
    let
        go : Msg -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
        go msg ( accM, accCmd ) =
            let
                ( m, cmd ) =
                    update msg accM
            in
            ( m, cmd :: accCmd )
    in
    List.foldl go ( { model | ws = ws }, [ Cmd.map WSMsg socketCmd ] ) msgs
        |> Tuple.mapSecond Cmd.batch



-- ---------------------------
-- VIEW
-- ---------------------------


view model =
    { title = "Websocket example"
    , body = [ view_ model ]
    }


view_ : Model -> Html Msg
view_ model =
    div [ class "container" ]
        [ div []
            [ button [ onClick Connect ] [ text "Connect" ]
            , button [ onClick Disconnect ] [ text "Disconnect" ]
            ]
        , button [ onClick Send ] [ text <| "send: message " ++ String.fromInt model.count ]
        , div [] [ text <| Debug.toString <| WebSocket.getStatus url model.ws ]
        , div [] [ text model.message ]
        ]



-- ---------------------------
-- Subscriptions
-- ---------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    fromSocket <| WebSocket.listen listeners WSMsg


listeners : Dict String (String -> Msg)
listeners =
    Dict.fromList [ ( url, OnEcho ) ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
