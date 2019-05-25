port module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import WebSocket exposing (PortMsg)


port fromJs : (WebSocket.PortMsg -> msg) -> Sub msg


port toJs : WebSocket.PortMsg -> Cmd msg



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
    ( { ws = WebSocket.init toJs, message = "", count = 1 }, Cmd.none )


url =
    "wss://echo.websocket.org"


listeners =
    Dict.singleton url OnEcho



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Send
    | Disconnect
    | Connect
    | WSMsg WebSocket.Msg
    | OnEcho String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnEcho str ->
            ( { model | message = str }, Cmd.none )

        Disconnect ->
            handleSocket (WebSocket.setSockets []) model

        Connect ->
            handleSocket (WebSocket.setSockets <| Dict.keys listeners) model

        Send ->
            handleSocket (WebSocket.send url <| "message: " ++ String.fromInt model.count)
                { model | count = model.count + 1 }

        WSMsg msg ->
            handleSocket (WebSocket.update msg) model


handleSocket : (WebSocket.State -> ( WebSocket.State, Cmd WebSocket.Msg )) -> Model -> ( Model, Cmd Msg )
handleSocket fn m =
    let
        ( ws, c ) =
            fn m.ws
    in
    ( { m | ws = ws }, Cmd.map WSMsg c )



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div []
            [ button [ onClick Disconnect ] [ text "Disconnect" ]
            , button [ onClick Connect ] [ text "Connect" ]
            ]
        , button [ onClick Send ] [ text <| "send: message " ++ String.fromInt model.count ]
        , div [] [ text <| Debug.toString <| WebSocket.getStatus url model.ws ]
        , div [] [ text model.message ]
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> fromJs <| WebSocket.listen listeners WSMsg
        }
