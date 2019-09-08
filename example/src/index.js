"use strict";

require("./styles.scss");
import {_WebSocket_handler} from "../../src/elm_websocket";

const { Elm } = require("./Main");
var app = Elm.Main.init();

// Socket - note the wrapping so that id the send function changes, the callback always works
app.ports.toSocket.subscribe(data => {
    _WebSocket_handler(data, msg => {
        app.ports.fromSocket.send(msg);
    });
});

