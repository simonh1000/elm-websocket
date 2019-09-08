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


wsConfig : WebSocket.Config
wsConfig =
    { toSocket = toSocket }



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
            handleSocket (WebSocket.setSockets wsConfig []) model

        Connect ->
            handleSocket (WebSocket.setSockets wsConfig <| Dict.keys listeners) model

        Send ->
            handleSocket (WebSocket.send wsConfig url <| "message: " ++ String.fromInt model.count)
                { model | count = model.count + 1 }

        WSMsg msg ->
            handleSocket (WebSocket.update wsConfig msg) model


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


view model =
    { title = "Elm 0.19 starter"
    , body = [ view_ model ]
    }


view_ : Model -> Html Msg
view_ model =
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
