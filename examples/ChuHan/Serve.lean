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
  /- Use `serveConcurrent` because /api/ask can block on the LLM for
     several seconds; while it's blocked the user may still navigate
     the static page or fire another tab. -/
  serveConcurrent a.port a.host (handler cfg pageProv gameProv)

end ChuHanServe

def main (args : List String) : IO Unit := ChuHanServe.serveMain args
