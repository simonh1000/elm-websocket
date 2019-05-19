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


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { ws = WebSocket.init toJs, message = "", count = 1 }, Cmd.none )


url =
    "ws://echo.websocket.org"


listeners =
    Dict.singleton url OnEcho


connect =
    handleSocket (WebSocket.setSockets [ url ])



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
    case Debug.log "update" message of
        OnEcho str ->
            ( { model | message = str }, Cmd.none )

        Disconnect ->
            handleSocket (WebSocket.setSockets []) model

        Connect ->
            connect model

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


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \m -> fromJs <| WebSocket.listen listeners WSMsg
        }
