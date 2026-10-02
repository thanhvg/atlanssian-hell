// ==UserScript==
// @name         Confluence Cloud bridge
// @namespace    browser-bridge
// @version      0.1.0
// @description  Lets a local server (ws://127.0.0.1:3979/ws) read Confluence Cloud through this logged-in tab.
// @match        https://*.atlassian.net/wiki/*
// @grant        GM_xmlhttpRequest
// @grant        unsafeWindow
// @connect      api.media.atlassian.com
// @connect      media.atlassian.com
// @connect      atlassian.com
// @connect      atlassian.net
// @noframes
// @run-at       document-idle
// ==/UserScript==

/*
 * Protocol (JSON, one object per websocket message):
 *   server -> page  {id, action, params}
 *   page -> server  {id, ok:true, result} | {id, ok:false, error}
 *   page -> server  {type:"hello", site, url, title, user, actions}  (on connect, every 20s)
 *
 * Read-only: no write actions are exposed.
 */
(function () {
  'use strict';

  const WS_URL = 'ws://127.0.0.1:3979/ws';
  const SITE = 'confluence';
  const API = '/wiki/rest/api';
  const HELLO_MS = 20000;
  const BACKOFF_MIN = 1000;
  const BACKOFF_MAX = 30000;

  const log = (...a) => console.log('%c[bridge]', 'color:#0052cc;font-weight:bold', ...a);

  // ---- same-origin helpers ----------------------------------------------

  /** Resolve P against this origin; refuse other origins and paths outside PREFIX. */
  function sameOriginUrl(p, prefix) {
    const u = new URL(p, location.origin);
    if (u.origin !== location.origin) throw new Error('cross-origin request refused');
    if (!u.pathname.startsWith(prefix)) throw new Error(`path must start with ${prefix}`);
    return u.href;
  }

  async function jsonGet(url) {
    const r = await fetch(url, {
      credentials: 'include',
      headers: { Accept: 'application/json' },
    });
    if (r.status === 401 || r.status === 403) {
      throw new Error(`HTTP ${r.status}: session expired or no access; reload the Confluence tab`);
    }
    const text = await r.text();
    if (!r.ok) throw new Error(`HTTP ${r.status}: ${text.slice(0, 300)}`);
    const ct = r.headers.get('content-type') || '';
    if (!ct.includes('json')) {
      throw new Error('non-JSON response (login page?); reload the Confluence tab');
    }
    return JSON.parse(text);
  }

  /**
   * Fetch URL as a Blob.
   *
   * Attachment URLs redirect to Atlassian's media service
   * (api.media.atlassian.com), which answers with CORS `*`.  A page-context
   * fetch cannot follow that redirect (the browser refuses `*` when
   * credentials are attached), so use Tampermonkey's GM_xmlhttpRequest,
   * which is not bound by CORS and follows redirects itself.  The plain
   * fetch below is only a fallback for managers without it.
   */
  function fetchBlob(url) {
    const u = new URL(url);
    const where = `${u.origin}${u.pathname}`;
    if (typeof GM_xmlhttpRequest === 'function') {
      return new Promise((resolve, reject) => {
        GM_xmlhttpRequest({
          method: 'GET',
          url,
          responseType: 'blob',
          timeout: 30000,
          onload: (r) =>
            r.status >= 200 && r.status < 300
              ? resolve(r.response)
              : reject(new Error(`HTTP ${r.status} for ${where}`)),
          onerror: () =>
            reject(new Error(
              `request failed for ${where}; if Tampermonkey asked to allow a domain, choose "Always allow"`)),
          ontimeout: () => reject(new Error(`timed out fetching ${where}`)),
        });
      });
    }
    return fetch(url, { credentials: 'same-origin' }).then((r) => {
      if (!r.ok) throw new Error(`HTTP ${r.status} for ${where}`);
      return r.blob();
    });
  }

  const apiGet = (path) => jsonGet(sameOriginUrl(`${API}${path}`, API));

  function pageIdFromLocation() {
    const m = location.pathname.match(/\/pages\/(\d+)/) || location.search.match(/[?&]pageId=(\d+)/);
    return m ? m[1] : null;
  }

  const PAGE_EXPAND = 'body.view,version,ancestors,space,metadata.labels';

  // ---- actions -----------------------------------------------------------

  const actions = {
    async ping() {
      return { url: location.href, user, pageId: pageIdFromLocation() };
    },

    /** The page this tab is showing, if any. */
    async current() {
      return { id: pageIdFromLocation(), url: location.href, title: document.title };
    },

    /** Generic read: any path under /wiki/rest/api/. */
    async get({ path }) {
      if (!path) throw new Error('get: missing `path`');
      return jsonGet(sameOriginUrl(path, API));
    },

    /** CQL search. */
    async search({ cql, limit = 25, start = 0 }) {
      if (!cql) throw new Error('search: missing `cql`');
      const q = new URLSearchParams({ cql, limit: String(limit), start: String(start) });
      return apiGet(`/search?${q}`);
    },

    /** Read a page (rendered HTML in body.view.value). */
    async page({ id }) {
      if (!/^\d+$/.test(String(id))) throw new Error('page: `id` must be numeric');
      return apiGet(`/content/${id}?expand=${PAGE_EXPAND}`);
    },

    /** Child pages. */
    async children({ id, start = 0, limit = 100 }) {
      if (!/^\d+$/.test(String(id))) throw new Error('children: `id` must be numeric');
      return apiGet(`/content/${id}/child/page?start=${start}&limit=${limit}`);
    },

    /** Attachment / image under /wiki/, returned as a data: URI. */
    async blob({ path }) {
      if (!path) throw new Error('blob: missing `path`');
      const b = await fetchBlob(sameOriginUrl(path, '/wiki/'));
      return new Promise((resolve, reject) => {
        const fr = new FileReader();
        fr.onload = () => resolve(fr.result);
        fr.onerror = () => reject(new Error('could not read blob'));
        fr.readAsDataURL(b);
      });
    },
  };

  // ---- connection --------------------------------------------------------

  let socket = null;
  let backoff = BACKOFF_MIN;
  let helloTimer = null;
  let reconnectTimer = null;
  let user = null;

  const send = (obj) => {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify(obj));
      return true;
    }
    return false;
  };

  function hello() {
    send({
      type: 'hello',
      site: SITE,
      url: location.href,
      title: document.title,
      user,
      actions: Object.keys(actions),
    });
  }

  async function handle(msg) {
    const { id, action, params } = msg || {};
    if (!id) return;
    const fn = actions[action];
    if (!fn) return void send({ id, ok: false, error: `unknown action: ${action}` });
    try {
      send({ id, ok: true, result: await fn(params || {}) });
    } catch (err) {
      send({ id, ok: false, error: (err && err.message) || String(err) });
    }
  }

  function connect() {
    clearTimeout(reconnectTimer);
    try {
      socket = new WebSocket(WS_URL);
    } catch (err) {
      log('WebSocket construction failed', err);
      return schedule();
    }
    socket.addEventListener('open', () => {
      log('connected');
      backoff = BACKOFF_MIN;
      hello();
      clearInterval(helloTimer);
      helloTimer = setInterval(hello, HELLO_MS);
    });
    socket.addEventListener('message', (ev) => {
      try {
        handle(JSON.parse(ev.data));
      } catch (_) {
        /* ignore non-JSON */
      }
    });
    socket.addEventListener('close', () => {
      clearInterval(helloTimer);
      schedule();
    });
  }

  function schedule() {
    clearTimeout(reconnectTimer);
    reconnectTimer = setTimeout(connect, backoff);
    backoff = Math.min(backoff * 2, BACKOFF_MAX);
  }

  // Learn the user's name once, for display in the server's tab list.
  apiGet('/user/current')
    .then((u) => {
      user = u.displayName || u.publicName || null;
      hello();
    })
    .catch(() => {});

  unsafeWindow.confluenceBridge = { actions, connect, get socket() { return socket; } };
  connect();
})();
