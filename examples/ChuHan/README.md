# 楚漢恋歌 — Chu-Han Love Song

A 2D narrative game set in BCE 209-195 China — the fall of Qin, the
Chu-Han contention, and the founding of Han. Six playable protagonists,
each ending in their historical death. Two-layer dialogue (outer speech
+ inner monologue) anchors the "charmer / egoist" axis. Three
mini-games (顔相鑑定 / 玉玦三度 / 背水之陣) replace key choices with
real interaction. Free-form NPC chat is LLM-driven via a local LM
Studio backend.

Built on top of `LeanTea` + `LeanJs` + DOM + Canvas 2D.

## Quick start

```bash
# 1. Build
lake build chuhan_serve

# 2. Serve. --dev hot-reloads Game.leanjs / page.html on every request.
./.lake/build/bin/chuhan_serve --dev

# 3. Open
open http://127.0.0.1:8050/
```

For the LLM-driven NPC chats:

```bash
# 1. Start LM Studio (or any OpenAI-compatible server) on port 11211
#    with a chat-capable model (qwen2.5 / gemma-3 / qwen3 work well).
# 2. Point the server at it:
LMSTUDIO_BASE_URL=http://127.0.0.1:11211/v1 \
  LMSTUDIO_MODEL=qwen/qwen2.5-7b-instruct \
  ./.lake/build/bin/chuhan_serve --dev
```

The LLM is consulted only for free-form NPC chat (the `★LLM自由会話★`
choices). All main plot beats are scripted; the LLM cannot break
canon. Character cards (in `Serve.lean`) enforce period (BCE 209-195),
forbid anachronism, and rebuff modern-knowledge prompts in-character.

## Characters & deaths

| Route | Theme | Wife / partner | Death (TRUE END) |
|---|---|---|---|
| 🟦 **劉邦** | 人たらしの仮面の下のエゴ | 呂雉 | 漢高祖 崩御 (BCE 195) |
| 🟥 **項羽** | 純なる暴君の悲劇 | 虞姫 | 烏江自刎 (BCE 202) |
| 🟨 **韓信** | 政の音痴な軍才 | 漂雲 (オリキャラ) | 長楽宮鐘室の処刑 (BCE 196) |
| 🟩 **張良** | 引退する賢者 | 黒綺 (オリキャラ) | 仙人の道 (BCE 189 頃) |
| 🟪 **蕭何** | 共犯者の罪 | 妙容 (オリキャラ) | 漢初の相国 (BCE 193) |
| 🟧 **范増** | 正論が届かぬ忠臣 | 妻 (故人) | 憤死 (BCE 約 204, 享年 75) |

## Mini-games

| Mini | When | What you do |
|---|---|---|
| **顔相鑑定** | 劉邦 Act 1 banquet | Compose 目つき・口元・姿勢 (3×3 = 9 combos). The "dragon face" combo unlocks bonus marriage |
| **玉玦三度** | 范増 Act 2 鴻門 | 3 timed clicks within a hit window. 3/3 → 項羽 actually strikes Liu Bang (historical divergence) |
| **背水之陣** | 韓信 Act 2 井陘 | 3-turn card sequence: 布陣 → 戦法 → 決戦. The right pattern (背水 → 黙 → 奇襲) scores 9/9 |

## LLM chat scenes (★ marked)

| Route | NPC | Card highlights |
|---|---|---|
| 劉邦 | 蕭何 | 沛の主吏、劉邦の嘘を見抜くが告発しない |
| 項羽 | 范増 | 七十の軍師、文語混じり、漢文体 |
| 韓信 | 蒯通 | 弁士、三国鼎立の説得役 |
| 張良 | 黄石公 | 仙人風、命令形と禅問答 |
| 蕭何 | 妙容 | 妻、家族の名誉を共犯者として差し出せる芯の強さ |
| 范増 | 項伯 | 項羽の叔父、張良に旧縁 |

Anachronism guard test: try typing "コンピュータ知ってる?" or
"明日の天気予報は?" — characters should reply with confused
in-period quips like "何の妖術じゃ?" or "酒の飲み過ぎでは?"

## Save system

- **Auto-save**: every click / keypress writes to `chuhan-save-v2`.
- **Checkpoint**: snapshot taken on phase change and before each
  mini-game; one slot at `chuhan-checkpoint`.
- **3 named slots**: `chuhan-slot-{1,2,3}`. Save / load via the 💾
  menu in the HUD.
- **Ending tracker**: each TRUE END / ★ 隠し END is recorded once
  achieved. The title screen shows "達成 END: N / total" and the
  「END ギャラリー」button reveals the full list.

All save state is `localStorage` — wipe with browser DevTools or by
opening the menu and choosing 「⟲ 最初からやり直す」.

## Test mode (jump to any scene)

```
http://127.0.0.1:8050/?scene=xiangyu_wujiang_death
http://127.0.0.1:8050/?scene=liubang_act3
http://127.0.0.1:8050/?scene=hanxin_xizheng
```

The character is inferred from the scene-id prefix; override with
`&char=…` if needed. Useful for QAing endings or jumping to a
specific minigame without replaying.

## File layout

| File | Lines | Purpose |
|---|---|---|
| `Game.leanjs` | ~3700 | All game logic — state, view, update, story content |
| `page.html`   | ~620  | CSS + thin JS shell (event delegation, RAF, fetch, localStorage externs) |
| `Serve.lean`  | ~210  | HTTP server, LeanJs compile pipeline, `/api/ask` LLM endpoint, 9 character cards |
| `Game.lean`   | ~30   | Loader for `Game.leanjs` |

## LeanJs gotchas (encountered while building)

1. **Numeric match patterns NOT supported** (`| 0 => …` fails). Use
   `if turn == 0 then … else …` chains.
2. **Match-arm body uses `parseAdd`** (no `let`/`if` directly). Wrap
   multi-statement arm bodies in `(…)`.
3. **String escapes don't work in `extern js "…"`**. `\"` terminates
   the string early. Use single quotes or typographic `“` `”` inside.
4. **Records: single-identifier types only** (`choices : List Choice`
   fails; use `choices : Array`).
5. **`let _ := f(...); body`** is the only sequencing idiom — there
   is no general `;` operator outside `let`.
6. **0-arg externs need a dummy parameter** (or call via inline
   helper) since `f()` with empty parens doesn't parse as a 0-arg
   call.

## Architecture flow

```
Browser
  ├── data-msg click → JS dispatcher → LeanJs update(msg, state) → render
  ├── "送信" button (LLM chat) → fetch /api/ask → LeanJs update with llmReply
  └── localStorage ←→ LeanJs externs (lsLoad / lsSave / lsRecordEnding)

Server (chuhan_serve)
  ├── GET /         → page.html template with compiled Game.leanjs spliced
  ├── GET /game.js  → just the compiled JS (for debugging)
  └── POST /api/ask → LeanTea.Llm.Openai → LMStudio
                       with character card + history + new message
```

## Tested LeanJs subset features

This game exercises: records (named fields), string-match arms,
include resolution (we don't actually use it — single file), FFI
externs, async / await (not used here), dot-access for FFI methods,
array literals, object literals, nested if-then-else, multi-arg
functions, paren expressions for grouping match-arm bodies.

Not used: imports, ctors with args (algebraic data types via
`inductive ... where | Cons(h,t)`), classes/instances.

## Run scripts

```bash
# Direct compile (test the LeanJs source compiles)
./.lake/build/bin/leanjs_compile examples/ChuHan/Game.leanjs > /tmp/game.js

# Smoke the running server
PORT=18050 ./.lake/build/bin/chuhan_serve &
sleep 1
curl -s http://127.0.0.1:18050/ | head -5
curl -s -X POST -H 'content-type: application/json' \
  -d '{"npcId":"xiaohe","sceneId":"liubang_act1","history":[],"message":"こんにちは"}' \
  http://127.0.0.1:18050/api/ask
```
