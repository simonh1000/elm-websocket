//window.addEventListener("offline", () => console.log("offline"));
//window.addEventListener("online", () => console.log("online"));

const __WS_BadSecurity = "BadSecurity";
const __WS_BadArgs = "BadArgs"; // e.g. the url does not host a socket server
const __WS_BadReason = "BadReason";
const __WS_BadCode = "BadCode";
const __WS_NotOpen = "NotOpen";
const __WS_BadString = "BadString";

export function _WebSocket_handler(data, cb) {
    // console.log("[_WebSocket_handler]", data);
    switch (data.tag) {
        case "open":
            _WebSocket_open(data.payload, cb);
            break;

        case "send":
            _WebSocket_send(data.payload, cb);
            break;

        case "close":
            _WebSocket_close(data.payload, cb);
            break;

        default:
            break;
    }
}

function _WebSocket_open(url, callback) {
    var socket;
    try {
        socket = new WebSocket(url);
    } catch (err) {
        console.error("[WS] ***** creation of websocket failed", url);
        return callback({
            tag: "BadOpen",
            payload: {
                url,
                error: err.name === "SecurityError" ? __WS_BadSecurity : __WS_BadArgs
            }
        });
    }

    socket.addEventListener("open", function(event) {
        let res = { tag: "GoodOpen", payload: { url, socket } };
        callback(res);
    });

    socket.addEventListener("message", function(event) {
        // console.log("[WS] message", event);
        callback({ tag: url, payload: event.data });
    });

    socket.addEventListener("error", function(event) {
        // console.log("Error", event);
        callback({
            tag: "error",
            payload: {
                url,
                readyState: event.target.readyState
            }
        });
    });

    socket.addEventListener("close", function(event) {
        // console.log(event);
        callback({
            tag: "close",
            payload: {
                url,
                code: event.code,
                reason: event.reason,
                wasClean: event.wasClean
            }
        });
    });
}

function _WebSocket_send({ socket, url, message }, callback) {
//    console.log("[_WebSocket_send] About to send", url, message);
    var resp = { tag: "GoodSend", payload: { url: url } };

    if (socket.readyState !== WebSocket.OPEN) {
        resp.tag = "BadSend";
        resp.payload.error = __WS_NotOpen;
        return callback(resp);
    }

    try {
        socket.send(message);
        return callback(resp);
    } catch (err) {
        console.log("BadSend", err);
        resp.tag = "BadSend";
        resp.payload.error = __WS_BadString;
        return callback(resp);
    }

    //    callback({ tag, payload: { url } });
}

function _WebSocket_close({uid, socket }, callback) {
    try {
        console.log("Attempting to close socket:", uid);
        socket.close(1000, uid);
    } catch (err) {
        return callback({
            tag: err.name === "SyntaxError" ? __WS_BadReason : __WS_BadCode,
            payload: null
        });
    }
    // we can rely on the close message
    //    callback({ tag: "ConfirmClose" });
}

function _WebSocket_bytesQueued(socket, callback) {
    callback({ tag: "BytesQueued", payload: socket.bufferedAmount });
}
