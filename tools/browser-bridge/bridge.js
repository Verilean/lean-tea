#!/usr/bin/env node
// browser-bridge — a thin JSON-RPC stdin/stdout server backed by Playwright.
//
// Lean side (LeanTea.Browser) spawns this as a child process, then talks
// to it line-by-line: each request is one JSON object on stdin, each
// response is one JSON object on stdout.
//
// Request:  {"id": 1, "method": "navigate", "params": {"url": "https://…"}}
// Response: {"id": 1, "result": {...}}   |   {"id": 1, "error": "…"}
//
// One bridge instance manages a single browser + page. To control more
// than one page from the Lean side, spawn more bridges. Keeping the
// surface narrow (1 page, no contexts) is the right cost for the
// "screenshot the english app, ask a vision model what it sees" case.

import { chromium } from 'playwright';
import readline from 'node:readline';
import fs from 'node:fs';
import path from 'node:path';

let browser = null;
let context = null;
let page    = null;

/** Default launch settings. `browser_open` (and the env var
 *  `LEANTEA_BROWSER_HEADLESS`) flips these — once set they persist
 *  across implicit ensurePage calls (e.g. when `navigate` is the
 *  first method invoked). */
const defaults = {
  headless: process.env.LEANTEA_BROWSER_HEADLESS === '1'
    || process.env.LEANTEA_BROWSER_HEADLESS === 'true',
  width:    1280,
  height:   800,
};

/** Lazy-init the browser the first time a method needs it. Honours
 *  `defaults` plus per-call overrides. */
async function ensurePage(opts = {}) {
  if (page) return page;
  const headless = (opts.headless !== undefined) ? opts.headless : defaults.headless;
  const width    = opts.width  || defaults.width;
  const height   = opts.height || defaults.height;
  browser = await chromium.launch({
    headless,
    args: ['--no-sandbox', '--disable-dev-shm-usage'],
  });
  context = await browser.newContext({
    viewport: { width, height },
    deviceScaleFactor: 1,
  });
  page = await context.newPage();
  return page;
}

/** Helpers that need a live page; throw otherwise. */
function requirePage() {
  if (!page) throw new Error('no page open; call open or navigate first');
  return page;
}

const methods = {
  /** Open / re-open the browser. Toggling `headless` while a page is
   *  already open closes the existing one and re-launches under the
   *  new mode. Per-call values also update the bridge defaults so
   *  subsequent implicit opens (via `navigate`) inherit them. */
  async open(p = {}) {
    if (p.headless !== undefined) defaults.headless = !!p.headless;
    if (p.width)                  defaults.width    = p.width;
    if (p.height)                 defaults.height   = p.height;
    if (page) { await closeAll(); }
    const pg = await ensurePage(p);
    return {
      width:    pg.viewportSize().width,
      height:   pg.viewportSize().height,
      headless: defaults.headless,
    };
  },

  /** Navigate to a URL, waiting for `domcontentloaded` by default. */
  async navigate({ url, waitUntil = 'domcontentloaded', timeout = 30000 }) {
    const pg = await ensurePage();
    await pg.goto(url, { waitUntil, timeout });
    return { url: pg.url(), title: await pg.title() };
  },

  /** Click an element by CSS selector. */
  async click({ selector, timeout = 5000 }) {
    const pg = requirePage();
    await pg.click(selector, { timeout });
    return { ok: true };
  },

  /** Click at viewport-relative pixel coordinates. Essential for
   *  canvas/WebGL games where there are no DOM elements to target —
   *  the LLM looks at a screenshot, picks an (x,y), we click there. */
  async clickXy({ x, y, button = 'left', clickCount = 1, delay = 0 }) {
    const pg = requirePage();
    await pg.mouse.click(x, y, { button, clickCount, delay });
    return { ok: true, x, y };
  },

  /** Type text into an input. */
  async fill({ selector, text, timeout = 5000 }) {
    const pg = requirePage();
    await pg.fill(selector, text, { timeout });
    return { ok: true };
  },

  /** Press a single key (e.g. "Enter", "Tab"). */
  async press({ key, selector }) {
    const pg = requirePage();
    if (selector) await pg.press(selector, key);
    else          await pg.keyboard.press(key);
    return { ok: true };
  },

  /** Wait for a selector to appear. */
  async waitFor({ selector, state = 'visible', timeout = 5000 }) {
    const pg = requirePage();
    await pg.waitForSelector(selector, { state, timeout });
    return { ok: true };
  },

  /** Plain text content of an element (or whole body). */
  async getText({ selector = 'body', timeout = 5000 }) {
    const pg = requirePage();
    const el = await pg.waitForSelector(selector, { state: 'attached', timeout });
    return { text: (await el.innerText()) || '' };
  },

  /** Inner HTML of an element. */
  async getHtml({ selector = 'body', timeout = 5000 }) {
    const pg = requirePage();
    const el = await pg.waitForSelector(selector, { state: 'attached', timeout });
    return { html: await el.innerHTML() };
  },

  /** Run JS in the page. Returns the JSON-serialisable result. */
  async evaluate({ expression }) {
    const pg = requirePage();
    const result = await pg.evaluate(expression);
    return { result };
  },

  /** Take a screenshot. Returns base64 PNG bytes plus dimensions.
   *  If `outputPath` is given, also writes the raw image bytes there
   *  (parent dirs are created). Useful when the caller wants a file
   *  on disk to hand to another process — e.g. curl-ing to a vision
   *  LLM — without having to base64-decode the JSON response. */
  async screenshot({ selector, fullPage = false, format = 'png', outputPath } = {}) {
    const pg = requirePage();
    let buf;
    if (selector) {
      const el = await pg.waitForSelector(selector, { state: 'visible' });
      buf = await el.screenshot({ type: format });
    } else {
      buf = await pg.screenshot({ fullPage, type: format });
    }
    if (outputPath) {
      fs.mkdirSync(path.dirname(outputPath), { recursive: true });
      fs.writeFileSync(outputPath, buf);
    }
    const vp = pg.viewportSize();
    return {
      base64:     buf.toString('base64'),
      mime:       format === 'jpeg' ? 'image/jpeg' : 'image/png',
      bytes:      buf.length,
      width:      vp.width,
      height:     vp.height,
      outputPath: outputPath || null,
    };
  },

  /** Current page metadata. */
  async info() {
    const pg = requirePage();
    return { url: pg.url(), title: await pg.title() };
  },

  /** Close the page + browser, then exit the bridge process so the
   *  parent's `child.wait()` returns promptly. Without the explicit
   *  exit we'd sit waiting for stdin EOF, which can take a while if
   *  the parent doesn't immediately drop the pipe — the bridge would
   *  then keep its (closed) browser handles around for nothing.
   *  Scheduled via setTimeout so the JSON-RPC response can flush to
   *  stdout before the process disappears. */
  async close() {
    await closeAll();
    setTimeout(() => process.exit(0), 50);
    return { ok: true };
  },
};

async function closeAll() {
  try { if (page)    await page.close();    } catch (_) {}
  try { if (context) await context.close(); } catch (_) {}
  try { if (browser) await browser.close(); } catch (_) {}
  page = context = browser = null;
}

const rl = readline.createInterface({ input: process.stdin });

// Serial queue. Playwright's `page` isn't safe to use from concurrent
// async callers; even if it were, the Lean side talks to us request-by-
// request, so a single in-flight job at a time is correct.
let queue = Promise.resolve();
let stdinClosed = false;

function enqueue(handler) {
  queue = queue.then(handler).catch(() => {});
  return queue;
}

rl.on('line', (line) => {
  if (!line.trim()) return;
  enqueue(async () => {
    let req;
    try { req = JSON.parse(line); }
    catch (_) {
      process.stdout.write(JSON.stringify({ id: null, error: 'malformed JSON' }) + '\n');
      return;
    }
    const id = req.id ?? null;
    const fn = methods[req.method];
    if (!fn) {
      process.stdout.write(JSON.stringify({ id, error: `unknown method: ${req.method}` }) + '\n');
      return;
    }
    try {
      const result = await fn(req.params || {});
      process.stdout.write(JSON.stringify({ id, result }) + '\n');
    } catch (e) {
      process.stdout.write(JSON.stringify({ id, error: e.message || String(e) }) + '\n');
    }
  });
});

rl.on('close', () => {
  stdinClosed = true;
  // Drain the queue before exiting — otherwise an in-flight navigate /
  // screenshot would be cancelled when the parent closes its stdin
  // pipe right after the last request.
  queue.then(async () => {
    await closeAll();
    process.exit(0);
  });
});

process.on('SIGINT',  async () => { await closeAll(); process.exit(0); });
process.on('SIGTERM', async () => { await closeAll(); process.exit(0); });

// Tell the parent we're ready.
process.stdout.write(JSON.stringify({ id: 'ready', result: { ready: true } }) + '\n');
