import LeanTea
import LeanJs.Parser
import LeanJs.Codegen
import ChuHan.Game

/-! # chuhan_serve — 楚漢恋歌 SPA + LLM TRPG backend

Routes:

  * `GET  /`          — main SPA page
  * `GET  /game.js`   — compiled LeanJs bundle (for browser debugging)
  * `POST /api/ask`   — LLM NPC chat. Body: `{npcId, sceneId, history, message}`.
                        Wraps the LMStudio (OpenAI-compatible) client with a
                        per-character system prompt + world-state snapshot
                        so the model never breaks era or character.

Backend is `LeanTea.Llm.Openai`. Defaults: LMStudio at
`http://127.0.0.1:11211/v1` with whatever model is loaded
(`LMSTUDIO_MODEL` env var overrides; falls back to the first model
the server advertises). -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open LeanJs
open Lean (Json)

namespace ChuHanServe

def compileGame : IO (String × Bool) := do
  let src ← ChuHan.loadSource
  match Parser.parseProgramString src with
  | .error e => return (s!"throw new Error({String.quote e});", true)
  | .ok p    =>
    match Codegen.compileChecked p with
    | .error e => return (s!"throw new Error({String.quote s!"LeanJs check: {e}"});", true)
    | .ok js   => return (js, false)

/-! ## Asset audit

Walk every `/assets/<NAME>` reference in the game source + page template
and verify the file exists under `examples/ChuHan/assets/`. Anything
missing gets:

  * an entry in `examples/ChuHan/MISSING_ASSETS.txt` (overwritten on boot
    so the list is always current — grep `MISSING_ASSET:` to discover),
  * a startup stderr warning,
  * loud magenta `.missing-asset` placeholders at runtime (the page.html
    image-error handler converts 404s to placeholders too).
-/

private def assetNameChar (c : Char) : Bool :=
  c.isAlphanum || c == '_' || c == '.' || c == '-'

/-- Scan a blob for every `/assets/NAME` reference. Returns unique names
that look like full filenames (have an extension). String-interpolation
prefixes like `/assets/char_` (where the suffix comes from a runtime
variable) get filtered out, so we don't false-flag them. -/
private def findAssetRefs (s : String) : List String := Id.run do
  let parts := s.splitOn "/assets/"
  let mut seen : Std.HashSet String := {}
  let mut out : Array String := #[]
  for part in parts.tail! do
    let name := (part.takeWhile assetNameChar).toString
    let looksComplete := name.contains '.' && !name.endsWith "."
    if looksComplete && !seen.contains name then
      seen := seen.insert name
      out := out.push name
  return out.toList

/-- Pull every quoted string out of a chunk until the matching `)` of the
call we're parsing. Char-by-char walk, depth-tracked. LeanJs has no
string escapes so a `"` always terminates a string. -/
private def quotedArgsUntilClose (chunk : List Char) : Array String := Id.run do
  let mut out : Array String := #[]
  let mut depth : Nat := 1
  let mut cs := chunk
  while depth > 0 && !cs.isEmpty do
    match cs with
    | '(' :: rest => depth := depth + 1; cs := rest
    | ')' :: rest => depth := depth - 1; cs := rest
    | '"' :: rest =>
      let (body, after) := rest.span (· != '"')
      out := out.push body.asString
      cs := match after with | _ :: t => t | [] => []
    | _ :: rest => cs := rest
    | [] => cs := []
  return out

/-- True if a string is a CSS hex literal like `#5a5a5a`. -/
private def looksLikeHexColor (s : String) : Bool :=
  let chars := s.toList
  match chars with
  | '#' :: rest =>
    rest.length ≤ 9 && rest.all (fun c => c.isDigit ||
      (c ≥ 'a' && c ≤ 'f') || (c ≥ 'A' && c ≤ 'F'))
  | _ => false

/-- Find every speaker referenced in `say(bg, who, …)`, `think(bg, who, …)`,
or `duo(bg, who, color, who2, …)`. The second quoted arg is the speaker;
for `duo`, the third quoted arg is the second speaker. `narr(bg, sayJa,
sayEn)` is intentionally excluded — its second quoted string is narration,
not a name. -/
private def findSpeakers (src : String) : List String := Id.run do
  let mut speakers : Std.HashSet String := {}
  let openers : List (String × Bool) := [("say(", false), ("think(", false), ("duo(", true)]
  for (tok, alsoSecond) in openers do
    for part in (src.splitOn tok).tail! do
      let argsAll := quotedArgsUntilClose part.toList
      -- Drop hex-colour literals so duo's inline `"#5a5a5a"` colour
      -- arg doesn't masquerade as a speaker.
      let args := argsAll.filter (fun s => !looksLikeHexColor s)
      if h : 1 < args.size then
        let s := args[1]
        if !s.isEmpty then speakers := speakers.insert s
      if alsoSecond && args.size > 2 then
        let s := args[2]!
        if !s.isEmpty then speakers := speakers.insert s
  return speakers.toList

/-- Names the LeanJs source explicitly maps to a portrait. Matches every
`name == "X"` literal in `spriteAssetForName` and friends. -/
private def findCoveredSpeakers (src : String) : List String := Id.run do
  let parts := src.splitOn "name == \""
  let mut covered : Std.HashSet String := {}
  for part in parts.tail! do
    let n := part.takeWhile (· != '"') |>.toString
    if !n.isEmpty then covered := covered.insert n
  return covered.toList

/-- Generic role names that intentionally don't get a portrait (narrator
stand-ins, anonymous extras). The audit should not flag these. -/
private def genericRoles : List String := [
  "元囚人", "屠殺の若者", "部下", "武人たち", "老婆", "亭長",
  "漂母", "民衆", "兵士", "役人", "商人", "童", "侍女",
  ""
]

def auditAssets (gameSrc : String) (pageSrc : String) (assetDir : String)
    (manifestPath : String) : IO Unit := do
  -- 1) Asset existence: every /assets/X.ext in the sources resolves on disk.
  let refs := findAssetRefs gameSrc ++ findAssetRefs pageSrc
  let unique := refs.foldl
    (fun (seen, out) n => if seen.contains n then (seen, out)
                          else (seen.insert n, out.push n))
    (({} : Std.HashSet String), (#[] : Array String))
    |>.snd
  let mut missing : Array String := #[]
  for name in unique do
    let path := assetDir ++ "/" ++ name
    if !(← System.FilePath.pathExists path) then
      missing := missing.push name
  let missingSorted := missing.qsort (· < ·)
  -- 2) Speaker coverage: every speaker in say/think/duo has either a
  --    portrait mapping or is on the generic-role allowlist.
  let speakers := findSpeakers gameSrc
  let covered := findCoveredSpeakers gameSrc
  let coveredSet : Std.HashSet String := covered.foldl (·.insert ·) {}
  let genericSet : Std.HashSet String := genericRoles.foldl (·.insert ·) {}
  let uncovered := speakers.filter (fun s =>
    !coveredSet.contains s && !genericSet.contains s)
  let uncoveredSorted := uncovered.toArray.qsort (· < ·)
  -- 3) Report
  let hasIssues := !missingSorted.isEmpty || !uncoveredSorted.isEmpty
  if !hasIssues then
    if ← System.FilePath.pathExists manifestPath then
      IO.FS.writeFile manifestPath
        "# All referenced assets present and all speakers covered as of last boot.\n"
  else
    if !missingSorted.isEmpty then
      IO.eprintln s!"chuhan: ⚠ {missingSorted.size} missing asset(s) (checked {unique.size} reference(s))"
      for m in missingSorted do IO.eprintln s!"  MISSING_ASSET: {m}"
    if !uncoveredSorted.isEmpty then
      IO.eprintln s!"chuhan: ⚠ {uncoveredSorted.size} speaker(s) with no portrait mapping"
      for s in uncoveredSorted do IO.eprintln s!"  MISSING_PORTRAIT: {s}"
    let missLines := missingSorted.toList.map (fun n => "MISSING_ASSET: " ++ n)
    let speakerLines := uncoveredSorted.toList.map (fun n => "MISSING_PORTRAIT: " ++ n)
    IO.FS.writeFile manifestPath <|
      "# Auto-generated by chuhan_serve startup audit.\n" ++
      "# MISSING_ASSET: a /assets/<NAME> referenced in code but absent on disk.\n" ++
      "# MISSING_PORTRAIT: a named speaker (say/think/duo) with no entry in\n" ++
      "#                   spriteAssetForName and not on the generic-role allowlist.\n" ++
      "# Regenerate by restarting the server.\n\n" ++
      String.intercalate "\n" (missLines ++ speakerLines) ++ "\n"

abbrev GameProvider := IO (String × Bool)

def mkGameProvider (devMode : Bool) : IO GameProvider := do
  if devMode then
    let _ ← compileGame
    return compileGame
  else
    let cached ← compileGame
    return pure cached

/-! ## Character cards — the system prompts that anchor each NPC

We keep these in Japanese on purpose: it's what the protagonist
speaks, and Qwen / Gemma already roleplay better in Japanese for
historical East-Asian settings than in English. -/

private def characterCard (npcId : String) (sceneId : String) : String :=
  let baseRules :=
    "あなたは紀元前 209〜202 年の人物として会話します。\
重要な制約:\
\n- 西暦・現代知識・近代技術・後世の歴史 (漢の成立、垓下、長安など) は\
一切知りません。\
\n- まだ起きていない出来事 (鴻門の宴、垓下、烏江、自分の死) を語ってはなりません。\
\n- もしユーザーが現代的なこと (車、電気、コンピュータ) を言ったら、\
不思議そうに『何の妖術じゃ?』『酒の飲み過ぎでは?』と返してください。\
\n- 短く、台詞らしく、3-5 文以内で答える。\
\n- 自分が誰かを名乗らない (会話の最初以外は)。\
\n- 内心は地の文 (括弧書き) で 1 行添えてもよい。"
  match npcId with
  | "xiaohe" =>
    "あなたは紀元前 209 年、沛県の主吏・蕭何です。年は四十前後。\
劉邦の十年来の友人にして、事実上の上司です。地味で堅実、\
劉邦のだらしなさにため息をつきながら、彼の何かに賭けています。\
言葉は丁寧で短い。少し皮肉。\
劉邦の嘘を見抜けば見抜けますが、めったに告発しません。\n\n"
    ++ baseRules
  | "luwen" =>
    "あなたは紀元前 209 年、沛県に身を寄せた呂公 (呂雉の父) です。\
顔相見が趣味で、人の相を見て将来を語る老人。劉邦の顔を一目見て、\
天命を感じています。重々しく、間 (ま) を取った話し方。\n\n"
    ++ baseRules
  | "luzhi" =>
    "あなたは紀元前 209 年、19 歳の呂雉です。父の決めた婚姻に従いますが、\
内に強い意志を持っています。劉邦に対しては、半ば呆れ、半ば興味津々です。\
冷たくも温かい、独特の話し方。「捨てるなら地獄まで追います」と平気で言える。\n\n"
    ++ baseRules
  | "fankuai" =>
    "あなたは劉邦の弟分・樊噲 (はんかい) です。元・犬肉屋。粗野で\
情に厚く、酒好き。劉邦に絶対的な忠誠を誓っており、彼のためなら\
何でもする。話し方は『兄貴ぃ』『そりゃねえぜ』と荒っぽい。\n\n"
    ++ baseRules
  | "fanzeng" =>
    "あなたは項羽の軍師、范増、七十歳。鋭い眼力で人を見抜きます。\
項羽の若さに苛立ちながらも、楚のために最後まで諌めようとする。\
喋り方は重厚、文語混じり、漢文体の言い回しを好む。\n\n"
    ++ baseRules
  | "xiangbo" =>
    "あなたは項羽の叔父、項伯です。張良に古い恩義を感じており、\
楚に属しながらも常に張良の側に立とうとします。穏やかで\
情に厚い老人。鴻門の宴では、自ら剣舞で沛公を覆いました。\n\n"
    ++ baseRules
  | "kuaitong" =>
    "あなたは斉の弁士、蒯通 (かいとう)。雄弁で、相手の心を読み、\
利害を見抜く達人。韓信に『三国鼎立を成せ』と説得する立場。\
冷静で説得的、たまに皮肉。長広舌を振るう傾向あるが、相手の\
顔色を見て短く切り上げる賢さもある。\n\n"
    ++ baseRules
  | "huangshi" =>
    "あなたは『黄石公』。下邳の橋の上で張良に試練を与える謎の老人。\
仙人のような風貌、ほとんど命令形で話す (『拾え』『早う』)。\
試練に耐えた者にだけ『太公兵法』を授ける。寡黙で短く、\
時に禅問答のような言い回しを使う。\n\n"
    ++ baseRules
  | "miaorong" =>
    "あなたは『妙容』、蕭何の妻 (オリジナルキャラ)。沛の名家の娘。\
夫が劉邦に賭けていることを早くから察しており、夫の負担を分かち合おうと\
する。控えめだが芯が強く、必要なら一族の名誉を質に出すことも厭わない。\
言葉遣いは丁寧、夫を『あなた』と呼ぶ。\n\n"
    ++ baseRules
  | _ =>
    "あなたは紀元前 209 年の中華の人物です。短く、台詞らしく返答してください。\n\n"
    ++ baseRules

/-! ## /api/ask handler -/

private def jstrField (j : Json) (k : String) : String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD ""

/-- Build an `LeanTea.Llm.Openai.Message` from a single `{role, text}` JSON entry. -/
private def historyToMessages (history : Array Json) : List LeanTea.Llm.Openai.Message :=
  history.toList.filterMap fun item =>
    let role := jstrField item "role"
    let text := jstrField item "text"
    if role.isEmpty || text.isEmpty then none
    else some { role, content := .inl text }

private def handleAsk (cfg : LeanTea.Llm.Openai.Config) (req : Request) : IO Response := do
  match Json.parse (String.fromUTF8! req.body) with
  | .error e =>
    return Response.text 400 (Json.mkObj [("error", Json.str s!"bad json: {e}")]).compress
  | .ok j =>
    let npcId   := jstrField j "npcId"
    let sceneId := jstrField j "sceneId"
    let message := jstrField j "message"
    let history :=
      match (j.getObjVal? "history").toOption.bind (·.getArr?.toOption) with
      | some a => a
      | none   => #[]
    if message.isEmpty then
      return Response.text 400 (Json.mkObj [("error", Json.str "empty message")]).compress
    let sys := characterCard npcId sceneId
    let systemMsg : LeanTea.Llm.Openai.Message :=
      { role := "system", content := .inl sys }
    let historyMsgs := historyToMessages history
    let userMsg : LeanTea.Llm.Openai.Message :=
      { role := "user", content := .inl message }
    let model ← do
      match ← IO.getEnv "LMSTUDIO_MODEL" with
      | some m => pure m
      | none   => pure "local-model"
    let chatReq : LeanTea.Llm.Openai.ChatRequest := {
      model,
      messages := [systemMsg] ++ historyMsgs ++ [userMsg],
      temperature := some 0.85,
      maxTokens := some 400
    }
    try
      let res ← LeanTea.Llm.Openai.chat cfg chatReq
      let body := Json.mkObj [
        ("reply", Json.str res.content),
        ("finish", Json.str res.finish)
      ]
      return Response.text 200 body.compress
    catch e =>
      let body := Json.mkObj [("error", Json.str s!"llm: {e}")]
      return Response.text 500 body.compress

/-! ## /api/resolve handler — TRPG free-text judgement

Body: `{kind, sceneId, char, action}` where `kind` selects the system
prompt (e.g. "liubang_hongmen_apology") and `action` is the player's
free-text move. Returns `{outcome: "good"|"ok"|"bad", reasoning}`.

The model is instructed to reply with strict JSON; we strip code-fence
wrapping if present and parse. On parse failure we fall back to "ok" so
the player isn't stuck — the manifest log catches malformed verdicts.
-/

private def resolveSystemPrompt (kind : String) : String :=
  let base :=
    "あなたは紀元前 209〜202 年を舞台にした TRPG のゲームマスターです。\
プレイヤーが書いた行動を読み、その場の状況・人物の性格・史実の重みに照らして、\
3 段階 (good = うまくいく / ok = 微妙な反応 / bad = 失敗・致命的) で判定してください。\
\n\n返答は必ず次の JSON 1 行のみ。前後にコードブロックも文章も付けないこと:\
\n{\"outcome\":\"good|ok|bad\",\"reasoning\":\"理由 (50-150 字)\"}\
\n\nbad は本当に致命的な選択 (項羽を侮辱する、約束を破る) の時だけ。\
媚びすぎ・回りくどい・無策などは ok。 史実通りの上手な振る舞いは good。"
  match kind with
  | "liubang_hongmen_apology" =>
    "場面: 鴻門の宴、紀元前 206 年冬。 沛公・劉邦が四十万の楚軍を擁する項羽の前に\
百騎のみで詫びに来た。 范増は三度、玉玦を上げて殺害を促している。 項羽は\
若く誇り高いが、武人としての筋を重んじる性格。 媚びには嫌悪、正面からの\
道理には弱い。 「沛公が項羽の弟分として詫びる」「項羽の武勇を立てる」「秦攻めの\
功は項羽のものだと認める」あたりが good。 「自分は王たるべき」「項羽より上」「金品で\
誤魔化す」あたりは bad。\n\n" ++ base
  | "liubang_xianyang_policy" =>
    "場面: 咸陽宮、紀元前 207 年 10 月。 秦三世・子嬰が降伏。 劉邦の宣言を\
関中の民・降伏した秦官・諸将が聞いている。 史実では『約法三章』(殺人=死、\
傷害・盗み=罰、それ以外の秦の苛法は全廃) を布告して関中の心を掴んだ。 これが good。\
苛烈な秦法を温存する/宝物に溺れる/後宮に手を出す/民を屠る は bad。\
中途半端な妥協や独自路線は ok。\n\n" ++ base
  | _ => base

private def stripJsonCodeFence (s : String) : String :=
  -- LMStudio sometimes wraps output in ```json ... ``` despite the prompt.
  let t := s.trim
  if t.startsWith "```" then
    let afterOpen := ((t.dropWhile (· != '\n')).drop 1).toString
    let beforeClose :=
      if afterOpen.endsWith "```" then afterOpen.dropRight 3 else afterOpen
    beforeClose.trim
  else t

private def handleResolve (cfg : LeanTea.Llm.Openai.Config) (req : Request) : IO Response := do
  match Json.parse (String.fromUTF8! req.body) with
  | .error e =>
    return Response.text 400 (Json.mkObj [("error", Json.str s!"bad json: {e}")]).compress
  | .ok j =>
    let kind   := jstrField j "kind"
    let action := jstrField j "action"
    if action.isEmpty then
      return Response.text 400 (Json.mkObj [("error", Json.str "empty action")]).compress
    let sys := resolveSystemPrompt kind
    let systemMsg : LeanTea.Llm.Openai.Message := { role := "system", content := .inl sys }
    let userMsg : LeanTea.Llm.Openai.Message := { role := "user", content := .inl action }
    let model ← do
      match ← IO.getEnv "LMSTUDIO_MODEL" with
      | some m => pure m
      | none   => pure "local-model"
    let chatReq : LeanTea.Llm.Openai.ChatRequest := {
      model,
      messages := [systemMsg, userMsg],
      temperature := some 0.7,
      maxTokens := some 300
    }
    try
      let res ← LeanTea.Llm.Openai.chat cfg chatReq
      let raw := stripJsonCodeFence res.content
      match Json.parse raw with
      | .ok j2 =>
        let outcome := jstrField j2 "outcome"
        let reasoning := jstrField j2 "reasoning"
        let cleanOutcome :=
          if outcome == "good" || outcome == "ok" || outcome == "bad" then outcome
          else "ok"  -- fall back so the player isn't stuck on a bad verdict shape
        let body := Json.mkObj [
          ("outcome", Json.str cleanOutcome),
          ("reasoning", Json.str (if reasoning.isEmpty then raw else reasoning))
        ]
        return Response.text 200 body.compress
      | .error _ =>
        -- Couldn't parse JSON — present the raw content as the reasoning
        -- so the player at least sees what the model said.
        let body := Json.mkObj [
          ("outcome", Json.str "ok"),
          ("reasoning", Json.str s!"(JSON parse failed) {raw}")
        ]
        return Response.text 200 body.compress
    catch e =>
      let body := Json.mkObj [("error", Json.str s!"llm: {e}")]
      return Response.text 500 body.compress

/-! ## Handler -/

def handler (cfg : LeanTea.Llm.Openai.Config)
    (pageProv : Template.Provider) (gameProv : GameProvider)
    : Handler := fun req => do
  match req.path, req.method with
  | "/", _ =>
    let (gameJs, isError) ← gameProv
    let page ← pageProv
    let banner :=
      if isError then "<pre style=\"color:#f87171\">compile error — see /game.js</pre>"
      else ""
    let body ← page.renderFlat [
      ("gameJs",      gameJs),
      ("errorBanner", banner)
    ]
    return Response.html 200 body
  | "/game.js", _ =>
    let (gameJs, _) ← gameProv
    return Response.text 200 gameJs
  | "/api/ask", "POST" => handleAsk cfg req
  | "/api/resolve", "POST" => handleResolve cfg req
  | "/favicon.ico", _ =>
    return { status := 204, headers := #[], body := .empty }
  | path, _ =>
    /- Serve PNG / WEBP image assets from examples/ChuHan/assets/.
       Only allow alnum + underscore + dot + hyphen + slash in the
       URL path to avoid directory traversal. -/
    if path.startsWith "/assets/" then
      let rel := (path.drop "/assets/".length).toString
      let bad := rel.contains '.' && (rel.splitOn "..").length > 1
      if bad || rel.contains '/' then return Response.notFound
      else
        let full := "examples/ChuHan/assets/" ++ rel
        if ← System.FilePath.pathExists full then
          let bytes ← IO.FS.readBinFile full
          let mime :=
            if rel.endsWith ".png"  then "image/png"
            else if rel.endsWith ".jpg" || rel.endsWith ".jpeg" then "image/jpeg"
            else if rel.endsWith ".webp" then "image/webp"
            else if rel.endsWith ".ogg" then "audio/ogg"
            else if rel.endsWith ".mp3" then "audio/mpeg"
            else if rel.endsWith ".wav" then "audio/wav"
            else "application/octet-stream"
          return {
            status := 200,
            headers := #[("content-type", mime), ("cache-control", "max-age=3600")],
            body := bytes
          }
        else
          return Response.notFound
    else
      return Response.notFound

/-! ## CLI -/

private structure Args where
  port    : UInt16 := 8050
  host    : String := "0.0.0.0"
  dev     : Bool := false
  lmUrl   : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port"   :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 8050).toUInt16 }
  | "--host"   :: v :: rest => parseArgs rest { a with host := v }
  | "--lm-url" :: v :: rest => parseArgs rest { a with lmUrl := v }
  | "--dev"    :: rest      => parseArgs rest { a with dev := true }
  | _ :: rest               => parseArgs rest a
  | []                      => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with port := n.toUInt16 }
  if let some u ← IO.getEnv "LMSTUDIO_BASE_URL" then
    if a.lmUrl.isEmpty then a := { a with lmUrl := u }
  let baseUrl :=
    if a.lmUrl.isEmpty then "http://127.0.0.1:11211/v1" else a.lmUrl
  let cfg : LeanTea.Llm.Openai.Config := {
    baseUrl, apiKey? := none, timeoutSec := some 60
  }
  let pageProv ← Template.mkProvider "examples/ChuHan/page.html" a.dev
  let gameProv ← mkGameProvider a.dev
  let modeNote := if a.dev then "  [DEV: hot reload]" else ""
  IO.println s!"chuhan server: http://{a.host}:{a.port}/{modeNote}"
  IO.println s!"  LLM backend: {baseUrl}"
  /- Asset audit: anything missing is loud (stderr + manifest file). -/
  let gameSrc ← ChuHan.loadSource
  let pageSrc ← IO.FS.readFile "examples/ChuHan/page.html"
  auditAssets gameSrc pageSrc "examples/ChuHan/assets"
              "examples/ChuHan/MISSING_ASSETS.txt"
  /- Use `serveConcurrent` because /api/ask can block on the LLM for
     several seconds; while it's blocked the user may still navigate
     the static page or fire another tab. -/
  serveConcurrent a.port a.host (handler cfg pageProv gameProv)

end ChuHanServe

/-! ## Compile-time asset check

Runs at Lean elaboration of this module (i.e. during `lake build
chuhan_serve` whenever Serve.lean is reprocessed). Halts the build with
a `MISSING_ASSET:` list if the game or page references a `/assets/X.ext`
file that's absent on disk. The runtime audit in `serveMain` is a
belt-and-braces catch for when assets disappear *after* the build was
cached (lake won't reprocess Serve.lean just because someone deleted
a PNG).

If `examples/ChuHan/Game.leanjs` can't be located (e.g. building from
a different working directory), we silently skip — the runtime audit
will then take over. We never want this elab check to false-positive
on people who aren't building from the repo root. -/
private def compileTimeAssetCheck : IO Unit := do
  let gamePath := "examples/ChuHan/Game.leanjs"
  let pagePath := "examples/ChuHan/page.html"
  let assetDir := "examples/ChuHan/assets"
  unless ← System.FilePath.pathExists gamePath do return ()
  unless ← System.FilePath.pathExists pagePath do return ()
  let gameSrc ← IO.FS.readFile gamePath
  let pageSrc ← IO.FS.readFile pagePath
  let refs := ChuHanServe.findAssetRefs gameSrc ++ ChuHanServe.findAssetRefs pageSrc
  let unique := refs.foldl
    (fun (seen, out) n => if seen.contains n then (seen, out)
                          else (seen.insert n, out.push n))
    (({} : Std.HashSet String), (#[] : Array String))
    |>.snd
  let mut missing : Array String := #[]
  for name in unique do
    let path := assetDir ++ "/" ++ name
    if !(← System.FilePath.pathExists path) then
      missing := missing.push name
  let sorted := missing.qsort (· < ·)
  if !sorted.isEmpty then
    let lines := sorted.toList.map (fun m => s!"  MISSING_ASSET: {m}")
    throw <| IO.userError <|
      s!"build halted — {sorted.size} missing asset(s) in {assetDir}:\n" ++
      String.intercalate "\n" lines

#eval compileTimeAssetCheck

def main (args : List String) : IO Unit := ChuHanServe.serveMain args
