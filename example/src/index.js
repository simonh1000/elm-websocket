"use strict";

require("./styles.scss");
import {_WebSocket_handler} from "../../src/elm_websocket";

const { Elm } = require("./Main");
var app = Elm.Main.init({ flags: 6 });

app.ports.toJs.subscribe(data => {
//    console.log("toJs", data);
    _WebSocket_handler(data, app.ports.fromJs.send);
});

