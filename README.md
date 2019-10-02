# Websocket

Aim: to reproduce as closely as possible the API of the 0.18 library.

Because of the need to use Ports, the amount of boilerplate is much higher. Beyond the boiler plate the only real difference is that you have declare your socket listeners as a Dict rather than calling `listen` multiple times.

## Installation (See the example for full details of what needs to be done)

1) Define ports and use these to create the required Config 

```
port fromSocket : (WebSocket.PortMsg -> msg) -> Sub msg
port toSocket : WebSocket.PortMsg -> Cmd msg
wsConfig : WebSocket.Config
wsConfig =
    { toJs = toJs }
```
    
The names do not matter, as you will have to wire them up anyway

2) Add `WebSocket.State` to Model

```
type alias Model =
    { socket : WebSocket.State
    , ...
    }
```

3) Add a `Msg` to wrap `WebSocket.Msg`

    ```
    type Msg
        = WSMsg WebSocket.Msg
        | OnEcho String
        | ...

    ```
    
4) Wire up the update function

```
WSMsg msg ->
    WebSocket.update wsConfig msg model.socket
       |> \(socket, cmd) -> ({ model | socket = socket }, Cmd.map WSMsg cmd )     
```
        
5) Connect subscriptions 

Here you pass the a list of sockets and the Msg you want websocket "messages" to be passed to your code. Other packets received on the socket will be handled by the WebSocket package itself.

```
listeners =
    Dict.fromList [ ( "ws://echo.websocket.org", OnEcho ) ]
    
Browser.document
    { subscriptions = \_ -> fromSocket <| WebSocket.listen listeners WSMsg
    , ...
    }
```

6) Open some sockets!

```
WebSocket.setSockets wsConfig (Dict.keys listeners) model.ws
    |> \(socket, cmd) -> ( { model | socket = socket }, Cmd.map WSMsg cmd )
``` 

7) [Javascript] copy elm_websocket.js in to your project, and connect to app. For example,

```js
import {_WebSocket_handler} from "./elm_websocket";

const { Elm } = require("./Main");
var app = Elm.Main.init();

app.ports.toSocket.subscribe(data => _WebSocket_handler(data, app.ports.fromSocket.send));
```

See `/example` for full code. 

## Notes

Setting and unsetting sockets

- 0.18 - the subscriptions chosen
- 0.19 - a specific function called in the update process

