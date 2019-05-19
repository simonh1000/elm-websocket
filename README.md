# Websockets

Aim: to reproduce as closely as possible the API of the 0.18 library.

Because of the need to use Ports, the amount of boilerplate is much higher.

## Installation

1) Define ports

    ```
    port fromJs : (WebSocket.PortMsg -> msg) -> Sub msg
    port toJs : WebSocket.PortMsg -> Cmd msg
    ```
    
    The names do not matter, as you will have to wire them up anyway

2) Add `WebSocket.State msg` to Model

    ```
    type alias Model =
        { ws : WebSocket.State Msg
        , ...
        }
    ```

3) Add a `Msg` to wrap `WebSocket.Msg`

    ```
    type Msg
        = WSMsg WebSocet.Msg
        | ...

    ```
    
4) Connect subscriptions 

    ```
    Browser.document
        { subscriptions = \m -> fromJs <| WebSocket.listen WSMsg m.ws
        , ...
        }
    ```

5) Wire up the update function

    ```
    WSMsg msg ->
        WebSocket.update msg model.ws
           |> \(ws, cmd) -> ({ model | ws = ws }, Cmd.map WSMsg cmd )     
    ```
        
6) Open some sockets!

    ```
    WebSocket.setSockets [ "ws://echo.websocket.org" ] model.ws
        |> \(ws, cmd) -> ( { model | ws = ws }, Cmd.map WSMsg cmd )
    ``` 

7) [Javascript] copy elm_websocket.js in to project, and connect to app. For example,

    ```js
    import {_WebSocket_handler} from "./elm_websocket";
    
    const { Elm } = require("./Main");
    var app = Elm.Main.init({ flags: null });
    
    app.ports.toJs.subscribe(data => {
        _WebSocket_handler(data, app.ports.fromJs.send);
    });
    ```


## Notes

Setting and unsetting sockets

- 0.18 - the subscriptions chosen
- 0.19 - a specific function called in the update process


Lifecycle
- opening 
    - triggered by user selecting the port; or
    - attempt to reconnect after unexpected closing
    - exponential backoff 
- open (route messages to client)
- closed by server / network down
- closed by client (does not exist, client must deselect client)