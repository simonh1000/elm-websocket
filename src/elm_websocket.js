//

const __WS_BadSecurity = "BadSecurity";
const __WS_BadArgs = "BadArgs";
const __WS_BadReason = "BadReason";
const __WS_BadCode = "BadCode";
const __WS_NotOpen = "NotOpen";
const __WS_BadString = "BadString";

export function _WebSocket_handler(data, cb) {
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
        console.log(err);
        return callback({
            tag: "BadOpen",
            payload: {
                url,
                error: err.name === "SecurityError" ? __WS_BadSecurity : __WS_BadArgs
            }
        });
    }

    socket.addEventListener("open", function(event) {
        callback({ tag: "GoodOpen", payload: { url, socket } });
    });

    socket.addEventListener("message", function(event) {
        callback({ tag: url, payload: event.data });
    });

    socket.addEventListener("error", function(event) {
        console.log(event);
        callback({
            tag: "error",
            payload: {
                url,
                readyState: event.target.readyState
            }
        });
    });

    socket.addEventListener("close", function(event) {
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
    var resp = {tag: "GoodSend", payload: {url: url}}

    if (socket.readyState !== WebSocket.OPEN) {
        resp.tag = "BadSend";
        resp.payload.reason = __WS_NotOpen;
        return callback(resp);
    }

    try {
        socket.send(message);
        return callback(resp);
    } catch (err) {
        resp.tag = "BadSend";
        resp.payload.reason = __WS_BadString;
        return callback(resp);
    }

    callback({ tag, payload: { url } });
}

function _WebSocket_close ({ reason, socket }, callback) {
    try {
        socket.close(1000, "user requested close");
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
    callback({tag: "BytesQueued", payload: socket.bufferedAmount});
}
