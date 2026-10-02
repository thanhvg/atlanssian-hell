// ==UserScript==
// @name         Jira Cloud bridge
// @namespace    browser-bridge
// @version      0.1.0
// @description  Lets a local server (ws://127.0.0.1:3979/ws) read Jira Cloud through this logged-in tab.
// @match        https://*.atlassian.net/browse/*
// @match        https://*.atlassian.net/jira/*
// @match        https://*.atlassian.net/issues/*
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
 * Same wire protocol as the Confluence bridge:
 *   server -> page  {id, action, params}
 *   page -> server  {id, ok:true, result} | {id, ok:false, error}
 *   page -> server  {type:"hello", site, url, title, user, actions}
 *
 * Read-only: no write actions are exposed.
 * Uses /rest/api/3/search/jql (the old /rest/api/3/search was removed).
 */
(function () {
  'use strict';

  const WS_URL = 'ws://127.0.0.1:3979/ws';
  const SITE = 'jira';
  const API = '/rest/api/';
  const API3 = '/rest/api/3';
  const HELLO_MS = 20000;
  const BACKOFF_MIN = 1000;
  const BACKOFF_MAX = 30000;

  const log = (...a) => console.log('%c[bridge]', 'color:#0052cc;font-weight:bold', ...a);

  // ---- same-origin helpers ----------------------------------------------

  /** Resolve P against this origin; refuse other origins and paths outside PREFIXES. */
  function sameOriginUrl(p, prefixes) {
    const u = new URL(p, location.origin);
    if (u.origin !== location.origin) throw new Error('cross-origin request refused');
    if (!prefixes.some((x) => u.pathname.startsWith(x))) {
      throw new Error(`path must start with one of: ${prefixes.join(', ')}`);
    }
    return u.href;
  }

  async function jsonGet(url) {
    const r = await fetch(url, {
      credentials: 'include',
      headers: { Accept: 'application/json' },
    });
    if (r.status === 401 || r.status === 403) {
      throw new Error(`HTTP ${r.status}: session expired or no access; reload the Jira tab`);
    }
    const text = await r.text();
    if (!r.ok) throw new Error(`HTTP ${r.status}: ${text.slice(0, 300)}`);
    const ct = r.headers.get('content-type') || '';
    if (!ct.includes('json')) {
      throw new Error('non-JSON response (login page?); reload the Jira tab');
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

  const apiGet = (path) => jsonGet(sameOriginUrl(`${API3}${path}`, [API]));

  const KEY_RE = /^[A-Za-z][A-Za-z0-9_]*-\d+$/;
  function requireKey(key) {
    if (!KEY_RE.test(String(key))) throw new Error(`invalid issue key: ${key}`);
    return encodeURIComponent(String(key).toUpperCase());
  }

  /** Issue key of the page this tab shows: /browse/KEY, ?selectedIssue=KEY, or .../issues/KEY. */
  function issueKeyFromLocation() {
    const m =
      location.pathname.match(/\/browse\/([A-Za-z][A-Za-z0-9_]*-\d+)/) ||
      location.search.match(/[?&]selectedIssue=([A-Za-z][A-Za-z0-9_]*-\d+)/) ||
      location.pathname.match(/\/issues\/([A-Za-z][A-Za-z0-9_]*-\d+)/);
    return m ? m[1].toUpperCase() : null;
  }

  const LIST_FIELDS = 'summary,status,assignee,priority,issuetype,updated,project';
  const ISSUE_FIELDS = [
    'summary', 'status', 'issuetype', 'priority', 'assignee', 'reporter', 'labels',
    'created', 'updated', 'description', 'project', 'components', 'fixVersions',
    'parent', 'attachment', 'issuelinks',
  ].join(',');

  // ---- actions -----------------------------------------------------------

  const actions = {
    async ping() {
      return { url: location.href, user, issue: issueKeyFromLocation() };
    },

    /** The issue this tab is showing, if any. */
    async current() {
      return { key: issueKeyFromLocation(), url: location.href, title: document.title };
    },

    async myself() {
      return apiGet('/myself');
    },

    /** Generic read: any path under /rest/api/. */
    async get({ path }) {
      if (!path) throw new Error('get: missing `path`');
      return jsonGet(sameOriginUrl(path, [API]));
    },

    /**
     * JQL search. Pages with nextPageToken (startAt no longer exists);
     * `fields` must be requested explicitly or only ids come back.
     */
    async search({ jql, limit = 50, nextPageToken }) {
      if (!jql) throw new Error('search: missing `jql`');
      const q = new URLSearchParams({
        jql,
        maxResults: String(Math.min(Number(limit) || 50, 100)),
        fields: LIST_FIELDS,
      });
      if (nextPageToken) q.set('nextPageToken', nextPageToken);
      return apiGet(`/search/jql?${q}`);
    },

    /** One issue; description is rendered HTML in renderedFields.description. */
    async issue({ key }) {
      return apiGet(`/issue/${requireKey(key)}?expand=renderedFields&fields=${ISSUE_FIELDS}`);
    },

    /** Comments with rendered HTML bodies (renderedBody). */
    async comments({ key, startAt = 0 }) {
      return apiGet(
        `/issue/${requireKey(key)}/comment?expand=renderedBody&orderBy=created&startAt=${startAt}&maxResults=100`
      );
    },

    /** Transitions currently available on an issue (read-only). */
    async transitions({ key }) {
      return apiGet(`/issue/${requireKey(key)}/transitions`);
    },

    /** Attachment / image under /rest/api/ or /secure/, returned as a data: URI. */
    async blob({ path }) {
      if (!path) throw new Error('blob: missing `path`');
      const b = await fetchBlob(sameOriginUrl(path, [API, '/secure/']));
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
  apiGet('/myself')
    .then((u) => {
      user = u.displayName || null;
      hello();
    })
    .catch(() => {});

  unsafeWindow.jiraBridge = { actions, connect, get socket() { return socket; } };
  connect();
})();
