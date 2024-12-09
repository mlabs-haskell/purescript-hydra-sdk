import PartySocket from "partysocket";
import WS from "ws";

export const _initWs = (url) => () => {
  const ws = new PartySocket({
    host: url,
    room: "main-room",
    WebSocket: WS
  });
  return ws;
};

export const _sendWs = (ws) => (msg) => () => {
  ws.send(msg);
};

export const _closeWs = (ws) => () => {
  if (typeof ws.removeAllListeners === "function") {
    ws.removeAllListeners();
  }
  ws.close();
};

export const _onWsConnect = (ws) => (cb) => () => {
  ws.addEventListener("open", (_ev) => {
    cb();
  });
};

export const _onWsMessage = (ws) => (cb) => () => {
  ws.addEventListener("message", (ev) => {
    if (ev.data instanceof ArrayBuffer || ev.data instanceof Blob) {
      console.log("_onWsMessage: unexpected binary message - ignoring");
      return;
    }
    cb(ev.data)();
  });
};

export const _onWsError = (ws) => (cb) => () => {
  ws.addEventListener("error", (ev) => {
    if ("message" in ev && typeof ev.message === "string" && ev.message.length > 0) {
      cb(ev.message)();
    } else if ("error" in ev && ev.error instanceof Error) {
      cb(ev.error.toString())();
    } else {
      cb(ev.toString())();
    }
  });
};

export const _onWsClose = (ws) => (cb) => () => {
  ws.addEventListener("close", (ev) => {
    cb(ev.code)(ev.reason)();
  });
};
