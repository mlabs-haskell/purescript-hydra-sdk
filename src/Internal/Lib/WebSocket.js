export const _onWsClose = (ws) => (cb) => () => {
  const listener = (ev) => cb(ev.code)(ev.reason)();
  ws.addEventListener("close", listener);
  ws.finalizers.push(() => {
    ws.removeEventListener("close", listener);
  });
};
