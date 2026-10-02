#!/usr/bin/env python3
"""browser-bridge server: relays typed requests to userscripts running in
logged-in browser tabs.  One port, one event loop (aiohttp):

    ws://127.0.0.1:3979/ws          userscripts connect here
    GET  http://127.0.0.1:3979/tabs list connected tabs
    POST http://127.0.0.1:3979/call {"site": "confluence", "action": "page",
                                     "params": {...}, "tab": "<id>"?, "timeout": 30?}

Site-agnostic: each tab announces itself with a hello message

    {"type": "hello", "site": "confluence", "url": "...", "user": "...",
     "actions": ["page", "search", ...]}

and the server only routes actions that tab declared.

Security model:
  * binds to 127.0.0.1 only
  * REST (everything except /ws) requires a secret token header, a Host
    header naming this server (blocks DNS rebinding), and no Origin header
    (browsers always send Origin on cross-origin requests; curl and Emacs
    don't), so web pages can't drive it
  * the websocket handshake checks Origin against an allow-list (browsers
    can't forge Origin; other local processes can, which is accepted for a
    single-user machine)

Requires: aiohttp >= 3.9
"""

from __future__ import annotations

import argparse
import asyncio
import json
import logging
import os
import re
import secrets
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple

from aiohttp import WSMsgType, web

log = logging.getLogger("bridge")

DEFAULT_HOST = "127.0.0.1"
DEFAULT_PORT = 3979
WS_PATH = "/ws"
MAX_WS_MESSAGE = 2**24  # 16 MiB; aiohttp's 4 MiB default is too small for big image blobs
TOKEN_FILE = Path(
    os.environ.get(
        "BRIDGE_TOKEN_FILE",
        str(Path.home() / ".config" / "browser-bridge" / "token"),
    )
)
DEFAULT_ORIGINS = [r"https://[a-z0-9-]+\.atlassian\.net"]


class BridgeError(RuntimeError):
    """The browser side reported an error."""


class NoClientError(BridgeError):
    """No matching tab is connected."""


class BridgeTimeout(BridgeError):
    """The tab did not answer in time."""


def load_token() -> str:
    if TOKEN_FILE.exists():
        return TOKEN_FILE.read_text().strip()
    TOKEN_FILE.parent.mkdir(parents=True, exist_ok=True)
    token = secrets.token_urlsafe(32)
    fd = os.open(TOKEN_FILE, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(fd, "w") as f:
        f.write(token + "\n")
    return token


@dataclass
class Client:
    ws: web.WebSocketResponse
    id: str = field(default_factory=lambda: uuid.uuid4().hex[:8])
    site: Optional[str] = None
    url: Optional[str] = None
    title: Optional[str] = None
    user: Optional[str] = None
    actions: Set[str] = field(default_factory=set)
    last_hello: float = 0.0
    pending: Set[str] = field(default_factory=set)

    def describe(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "site": self.site,
            "url": self.url,
            "title": self.title,
            "user": self.user,
            "actions": sorted(self.actions),
        }


class Bridge:
    def __init__(self) -> None:
        self._clients: Dict[str, Client] = {}
        # request id -> (client id, future); the client id lets us reject
        # responses that come from a tab the request was never sent to.
        self._pending: Dict[str, Tuple[str, asyncio.Future]] = {}

    # -- connections -------------------------------------------------------

    def register(self, ws: web.WebSocketResponse) -> Client:
        client = Client(ws=ws)
        self._clients[client.id] = client
        log.info("tab %s connected (%d total)", client.id, len(self._clients))
        return client

    def unregister(self, client: Client) -> None:
        self._clients.pop(client.id, None)
        # Fail in-flight requests immediately instead of letting them
        # sit until their timeout.
        for rid in list(client.pending):
            entry = self._pending.pop(rid, None)
            if entry and not entry[1].done():
                entry[1].set_exception(NoClientError("tab disconnected"))
        log.info("tab %s disconnected (%d left)", client.id, len(self._clients))

    def clients(self) -> List[Client]:
        return list(self._clients.values())

    def _pick(self, site: Optional[str], tab: Optional[str]) -> Client:
        if tab:
            c = self._clients.get(tab)
            if c is None:
                raise NoClientError(f"no connected tab with id {tab!r}")
            return c
        candidates = [
            c for c in self._clients.values() if c.site and (site is None or c.site == site)
        ]
        if not candidates:
            what = f"{site} tab" if site else "tab"
            raise NoClientError(
                f"no {what} connected; open the site in your browser "
                "with the userscript enabled"
            )
        return max(candidates, key=lambda c: c.last_hello)

    # -- incoming ----------------------------------------------------------

    async def on_message(self, client: Client, raw: str) -> None:
        try:
            msg = json.loads(raw)
        except (ValueError, TypeError):
            log.warning("tab %s sent non-JSON", client.id)
            return
        if not isinstance(msg, dict):
            return

        if msg.get("type") == "hello":
            client.site = msg.get("site")
            client.url = msg.get("url")
            client.title = msg.get("title")
            client.user = msg.get("user")
            acts = msg.get("actions")
            client.actions = set(acts) if isinstance(acts, list) else set()
            client.last_hello = time.time()
            return

        rid = msg.get("id")
        entry = self._pending.get(rid) if rid else None
        if entry is None or entry[0] != client.id:
            log.debug("ignoring response %r from tab %s", rid, client.id)
            return
        self._pending.pop(rid, None)
        client.pending.discard(rid)
        fut = entry[1]
        if fut.done():
            return
        if msg.get("ok"):
            fut.set_result(msg.get("result"))
        else:
            fut.set_exception(BridgeError(msg.get("error") or "unknown browser error"))

    # -- request/response --------------------------------------------------

    async def call(
        self,
        action: str,
        params: Optional[Dict[str, Any]] = None,
        *,
        site: Optional[str] = None,
        tab: Optional[str] = None,
        timeout: float = 30.0,
    ) -> Any:
        target = self._pick(site, tab)
        if target.actions and action not in target.actions:
            raise ValueError(
                f"tab {target.id} ({target.site}) has no action {action!r}; "
                f"available: {sorted(target.actions)}"
            )
        if target.ws.closed:
            raise NoClientError("tab disconnected")

        rid = uuid.uuid4().hex
        fut: asyncio.Future = asyncio.get_running_loop().create_future()
        self._pending[rid] = (target.id, fut)
        target.pending.add(rid)
        try:
            await target.ws.send_str(
                json.dumps({"id": rid, "action": action, "params": params or {}})
            )
            return await asyncio.wait_for(fut, timeout=timeout)
        except asyncio.TimeoutError as exc:
            raise BridgeTimeout(
                f"timed out after {timeout}s waiting for {action} on tab {target.id}"
            ) from exc
        except ConnectionError as exc:  # aiohttp: sending on a closing socket
            raise NoClientError("tab disconnected") from exc
        finally:
            self._pending.pop(rid, None)
            target.pending.discard(rid)


# ---------------------------------------------------------------------------
# HTTP app
# ---------------------------------------------------------------------------


def _err(code: int, message: str) -> web.Response:
    return web.json_response({"error": message}, status=code)


def make_app(bridge: Bridge, token: str, origins: List[re.Pattern], port: int) -> web.Application:
    allowed_hosts = {f"127.0.0.1:{port}", f"localhost:{port}"}

    @web.middleware
    async def guard(request: web.Request, handler: Any) -> web.StreamResponse:
        if request.path == WS_PATH:
            return await handler(request)  # origin-checked in the route itself
        if request.headers.get("Host") not in allowed_hosts:
            return _err(403, "bad Host header")
        if "Origin" in request.headers:
            return _err(403, "browser-originated requests are not allowed")
        supplied = request.headers.get("X-Bridge-Token") or ""
        if not secrets.compare_digest(supplied, token):
            return _err(403, "missing or invalid X-Bridge-Token")
        try:
            return await handler(request)
        except web.HTTPException as exc:  # 404/405 etc.: keep every reply JSON
            return _err(exc.status, exc.reason)

    async def ws_route(request: web.Request) -> web.StreamResponse:
        origin = request.headers.get("Origin")
        if not origin or not any(p.fullmatch(origin) for p in origins):
            log.warning("rejected websocket from origin %r", origin)
            return _err(403, "origin not allowed")
        ws = web.WebSocketResponse(max_msg_size=MAX_WS_MESSAGE)
        await ws.prepare(request)
        client = bridge.register(ws)
        try:
            async for msg in ws:
                if msg.type == WSMsgType.TEXT:
                    await bridge.on_message(client, msg.data)
                elif msg.type == WSMsgType.ERROR:
                    log.warning("tab %s websocket error: %s", client.id, ws.exception())
        finally:
            bridge.unregister(client)
        return ws

    async def tabs_route(request: web.Request) -> web.Response:
        return web.json_response({"ok": True, "result": [c.describe() for c in bridge.clients()]})

    async def call_route(request: web.Request) -> web.Response:
        if request.content_type != "application/json":
            return _err(415, "Content-Type must be application/json")
        try:
            body = await request.json()
            if not isinstance(body, dict) or not isinstance(body.get("action"), str):
                raise ValueError("body must be an object with a string `action`")
            params = body.get("params") or {}
            if not isinstance(params, dict):
                raise ValueError("`params` must be an object")
            timeout = min(float(body.get("timeout") or 30), 120.0)
        except (ValueError, TypeError) as exc:
            return _err(400, str(exc))

        try:
            result = await bridge.call(
                body["action"], params, site=body.get("site"), tab=body.get("tab"), timeout=timeout
            )
        except ValueError as exc:
            return _err(400, str(exc))
        except NoClientError as exc:
            return _err(503, str(exc))
        except BridgeTimeout as exc:
            return _err(504, str(exc))
        except BridgeError as exc:
            return _err(502, str(exc))
        except Exception as exc:  # noqa: BLE001 - never leave the client hanging
            log.exception("unexpected error")
            return _err(500, f"internal error: {exc}")
        return web.json_response({"ok": True, "result": result})

    app = web.Application(middlewares=[guard])
    app.add_routes(
        [
            web.get(WS_PATH, ws_route),
            web.get("/tabs", tabs_route),
            web.post("/call", call_route),
        ]
    )
    return app


# ---------------------------------------------------------------------------


async def amain(args: argparse.Namespace) -> None:
    token = load_token()
    log.info("auth token is in %s", TOKEN_FILE)
    origins = [re.compile(p) for p in (args.origin or DEFAULT_ORIGINS)]
    app = make_app(Bridge(), token, origins, args.port)
    runner = web.AppRunner(app, access_log=None)
    await runner.setup()
    try:
        await web.TCPSite(runner, args.host, args.port).start()
        log.info("listening on http://%s:%d (websocket at %s)", args.host, args.port, WS_PATH)
        await asyncio.Event().wait()
    finally:
        await runner.cleanup()


def main() -> None:
    p = argparse.ArgumentParser(description="browser bridge server")
    p.add_argument("--host", default=DEFAULT_HOST)
    p.add_argument("--port", type=int, default=DEFAULT_PORT)
    p.add_argument(
        "--origin",
        action="append",
        help="regex of allowed websocket Origin (repeatable); "
        f"default {DEFAULT_ORIGINS[0]!r}",
    )
    p.add_argument("-v", "--verbose", action="store_true")
    args = p.parse_args()
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )
    try:
        asyncio.run(amain(args))
    except KeyboardInterrupt:
        log.info("shutting down")


if __name__ == "__main__":
    main()
