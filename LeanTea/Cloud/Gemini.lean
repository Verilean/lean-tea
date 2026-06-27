import LeanTea.Net.SafePath
import LeanTea.Json.Helpers
import Lean.Data.Json

/-! # LeanTea.Cloud.Gemini — Google Gemini API client

`v1beta` REST 経由で Gemini API を叩く小さなクライアント。Lean
には TLS が無いので HTTPS は `curl(1)` shell-out。同パターンで動
いてる `LeanTea.Auth.OAuth2` を踏襲。

## モデル一覧 (2026 年現在)

| モデル | input ctx | 用途 |
|---|---|---|
| `gemini-2.5-pro` (default) | 2,097,152 | 重い code review / アーキレビュー |
| `gemini-2.5-flash` | 1,048,576 | 高速・コスト重視 |
| `gemini-2.5-flash-lite` | 1,048,576 | サマリ / 軽量 lint |

長いコンテキストを活かしたい場合は `Gemini.reviewMany` —
`SafePath` 経由でファイルを束ねて読み込み、相対パス付きで一括投
入する API を使う。

## 認証

`GEMINI_API_KEY` 環境変数を読む。AI Studio
(https://aistudio.google.com/apikey) で発行。free tier は
2.5-flash が 15 req/min / 1500 req/day。

## 例

```lean
let cfg ← Gemini.Config.fromEnv!
let resp ← Gemini.ask cfg "Hello, who are you?" {}
IO.println resp.text
IO.println s!"tokens in={resp.usage.input} out={resp.usage.output}"
```

```lean
-- 多くのファイルをまとめてレビュー
let resp ← Gemini.reviewMany cfg
  (workspace := "/path/to/repo")
  (paths := #["LeanTea/Net/Http.lean", "LeanTea/Net/Server.lean"])
  (prompt := "アーキテクチャレビュー: 設計上の弱点を指摘")
  {}
```
-/

namespace LeanTea.Cloud.Gemini

open LeanTea.Net (SafePath)
open Lean (Json)

/-! ## Config -/

structure Config where
  apiKey   : String
  /-- default `gemini-2.5-pro`。呼び出しごとに上書き可。 -/
  model    : String := "gemini-2.5-pro"
  endpoint : String := "https://generativelanguage.googleapis.com/v1beta"
  /-- 最大 output token。default はモデル既定。 -/
  maxOutputTokens : Option Nat := none
  /-- temperature (0.0 = deterministic). default はモデル既定。 -/
  temperature : Option Float := none
  /-- curl のタイムアウト秒。長コンテキストは生成時間がかかる
      ので default 300s = 5 分。 -/
  timeoutSec : Nat := 300
  deriving Inhabited, Repr

/-- 環境変数から Config を組み立て。`GEMINI_API_KEY` 必須、
    `GEMINI_MODEL` / `GEMINI_ENDPOINT` は任意。 -/
def Config.fromEnv? : IO (Option Config) := do
  match ← IO.getEnv "GEMINI_API_KEY" with
  | none => return none
  | some key =>
    let model := (← IO.getEnv "GEMINI_MODEL").getD "gemini-2.5-pro"
    let endpoint := (← IO.getEnv "GEMINI_ENDPOINT").getD
      "https://generativelanguage.googleapis.com/v1beta"
    return some { apiKey := key, model, endpoint }

/-- panic 変種。CI で「key 設定漏れ」を早期に止めるのに便利。 -/
def Config.fromEnv! : IO Config := do
  match ← Config.fromEnv? with
  | some c => return c
  | none   => throw (IO.userError
    "Gemini.Config.fromEnv!: GEMINI_API_KEY が設定されてません")

/-! ## Request / Response 型 -/

/-- 1 メッセージ (multi-turn chat 用)。 -/
structure Message where
  /-- `"user"` or `"model"` -/
  role : String
  text : String
  deriving Inhabited, Repr

/-- 呼び出しオプション。すべて optional で `{}` で OK。 -/
structure CallOpts where
  /-- Config.model を上書き。 -/
  model         : Option String := none
  /-- system prompt。Gemini では `systemInstruction` フィールドに行く。 -/
  system        : Option String := none
  /-- maxOutputTokens 上書き。 -/
  maxOutputTokens : Option Nat := none
  /-- temperature 上書き。 -/
  temperature   : Option Float := none
  /-- response JSON schema (構造化出力)。 -/
  jsonSchema    : Option Json := none
  deriving Inhabited

structure Usage where
  input  : Nat := 0
  output : Nat := 0
  total  : Nat := 0
  deriving Inhabited, Repr

structure Response where
  text  : String
  usage : Usage
  /-- 完了理由 (`STOP` / `MAX_TOKENS` / `SAFETY` / …)。 -/
  finishReason : String := ""
  /-- 使ったモデル名 (呼び出し時点での解決値)。 -/
  model : String := ""
  /-- raw レスポンス body — デバッグ・ログ用。 -/
  raw : String := ""
  deriving Inhabited, Repr

/-! ## curl wrapper — TLS が必要なので shell out -/

private structure CurlResp where
  status : Nat
  body   : String
  err    : String := ""

/-- POST JSON body to URL via curl, return status + body.
    Body は temp file 経由で渡す (巨大 prompt + binary safe)。 -/
private def curlPost (url : String) (jsonBody : String) (timeoutSec : Nat)
    : IO CurlResp := do
  let tmpDir := (← IO.getEnv "TMPDIR").getD "/tmp"
  let bodyFile := s!"{tmpDir}/gemini-req-{← IO.rand 0 0xffff_ffff}.json"
  let respFile := s!"{tmpDir}/gemini-resp-{← IO.rand 0 0xffff_ffff}.json"
  IO.FS.writeFile bodyFile jsonBody
  let out ← IO.Process.output {
    cmd := "curl",
    args := #[
      "-sS",
      "--max-time", toString timeoutSec,
      "-X", "POST",
      "-H", "content-type: application/json",
      "-w", "\n___STATUS:%{http_code}",
      "-o", respFile,
      "--data-binary", s!"@{bodyFile}",
      url
    ]
  }
  try IO.FS.removeFile bodyFile catch _ => pure ()
  let body ← (try IO.FS.readFile respFile catch _ => pure "")
  try IO.FS.removeFile respFile catch _ => pure ()
  let parts := out.stdout.splitOn "\n___STATUS:"
  let status := match parts with
    | [_, codeS] => codeS.trimAscii.toString.toNat?.getD 0
    | _ => 0
  return { status, body, err := out.stderr }

/-! ## Request body builder -/

private def jObj (xs : List (String × Json)) : Json := Json.mkObj xs
private def jArr (xs : List Json) : Json := Json.arr xs.toArray

/-- `Json.num` takes a `JsonNumber`; encode floats by `toString`+reparse.
    Good enough for temperature / topP. Matches the trick used in
    `LeanTea.Llm.Openai`. -/
private def floatJson (f : Float) : Json :=
  match Json.parse (toString f) with
  | .ok j    => j
  | .error _ => Json.num 0

private def buildRequest (messages : List Message) (opts : CallOpts) : Json := Id.run do
  let contents := messages.map fun m =>
    jObj [("role", Json.str m.role), ("parts", jArr [jObj [("text", Json.str m.text)]])]
  let mut fields : List (String × Json) := [
    ("contents", jArr contents)
  ]
  if let some sys := opts.system then
    fields := fields ++ [("systemInstruction",
      jObj [("parts", jArr [jObj [("text", Json.str sys)]])])]
  let mut gc : List (String × Json) := []
  if let some n := opts.maxOutputTokens then
    gc := gc ++ [("maxOutputTokens", Json.num (Int.ofNat n))]
  if let some t := opts.temperature then
    gc := gc ++ [("temperature", floatJson t)]
  if let some sch := opts.jsonSchema then
    gc := gc ++ [
      ("responseMimeType", Json.str "application/json"),
      ("responseSchema", sch)]
  if !gc.isEmpty then fields := fields ++ [("generationConfig", jObj gc)]
  return jObj fields

/-! ## Response parser -/

private def parseResponse (model rawBody : String) : IO Response := do
  match Json.parse rawBody with
  | .error e => throw <| IO.userError s!"Gemini: bad JSON ({e})\n{rawBody.take 500}"
  | .ok j =>
    -- API error envelope
    match (j.getObjVal? "error").toOption with
    | some err =>
      let msg := err.getStrD "message"
      let code := err.getNatD "code"
      throw <| IO.userError s!"Gemini API error {code}: {msg}"
    | none => pure ()
    -- candidates[0].content.parts[*].text を結合
    let candidates := (j.getObjVal? "candidates").toOption.getD Json.null
    let arr := match candidates.getArr? with | .ok a => a.toList | .error _ => []
    let cand := arr.head?.getD Json.null
    let content := (cand.getObjVal? "content").toOption.getD Json.null
    let parts := (content.getObjVal? "parts").toOption.getD Json.null
    let partArr := match parts.getArr? with | .ok a => a.toList | .error _ => []
    let text := String.intercalate "" (partArr.map (·.getStrD "text"))
    let finishReason := cand.getStrD "finishReason"
    let usageM := (j.getObjVal? "usageMetadata").toOption.getD Json.null
    let usage : Usage := {
      input  := usageM.getNatD "promptTokenCount",
      output := usageM.getNatD "candidatesTokenCount",
      total  := usageM.getNatD "totalTokenCount"
    }
    return { text, usage, finishReason, model, raw := rawBody }

/-! ## Core: generate -/

/-- メッセージ列 (chat history) を投げて 1 turn 分の応答を取る。 -/
def generate (cfg : Config) (messages : List Message) (opts : CallOpts := {})
    : IO Response := do
  let model := opts.model.getD cfg.model
  let url := s!"{cfg.endpoint}/models/{model}:generateContent?key={cfg.apiKey}"
  let body := (buildRequest messages opts).compress
  let resp ← curlPost url body cfg.timeoutSec
  if resp.status < 200 || resp.status >= 300 then
    throw <| IO.userError s!"Gemini HTTP {resp.status}: {resp.body.take 500}\n{resp.err}"
  parseResponse model resp.body

/-! ## Convenience APIs -/

/-- 単発質問。 -/
def ask (cfg : Config) (prompt : String) (opts : CallOpts := {})
    : IO Response :=
  generate cfg [{ role := "user", text := prompt }] opts

/-- chat — 履歴 + 新メッセージ。`history` は `[("user", "..."), ("model", "..."), ...]`
    の交互ペアになっているのが Gemini の期待。 -/
def chat (cfg : Config) (history : List Message) (message : String)
    (opts : CallOpts := {}) : IO Response :=
  generate cfg (history ++ [{ role := "user", text := message }]) opts

/-! ## Holistic codebase review

Gemini Pro の 2M token を活かす本命 API。`SafePath` で workspace
の外を読まないことを保証しつつ、複数ファイルを Markdown 形式で
prompt に束ねて一括投入する。 -/

/-- 拡張子 → fenced code block の言語ヒント。 -/
private def guessLang (path : String) : String :=
  let lc := path.toLower
  if      lc.endsWith ".lean" then "lean"
  else if lc.endsWith ".go"   then "go"
  else if lc.endsWith ".rs"   then "rust"
  else if lc.endsWith ".ts"   then "ts"
  else if lc.endsWith ".tsx"  then "tsx"
  else if lc.endsWith ".js"   then "js"
  else if lc.endsWith ".jsx"  then "jsx"
  else if lc.endsWith ".py"   then "python"
  else if lc.endsWith ".java" then "java"
  else if lc.endsWith ".c" || lc.endsWith ".h" then "c"
  else if lc.endsWith ".cpp" || lc.endsWith ".cc" || lc.endsWith ".hpp" then "cpp"
  else if lc.endsWith ".cs"   then "csharp"
  else if lc.endsWith ".kt"   then "kotlin"
  else if lc.endsWith ".swift" then "swift"
  else if lc.endsWith ".rb"   then "ruby"
  else if lc.endsWith ".php"  then "php"
  else if lc.endsWith ".sh"   then "bash"
  else if lc.endsWith ".sql"  then "sql"
  else if lc.endsWith ".yaml" || lc.endsWith ".yml" then "yaml"
  else if lc.endsWith ".toml" then "toml"
  else if lc.endsWith ".json" then "json"
  else if lc.endsWith ".html" then "html"
  else if lc.endsWith ".css"  then "css"
  else if lc.endsWith ".md"   then "markdown"
  else if lc.endsWith ".proto" then "protobuf"
  else if lc.endsWith ".tf"   then "hcl"
  else if lc.endsWith ".dockerfile" || lc.endsWith "Dockerfile" then "dockerfile"
  else ""

/-- 1 ファイルを `## File: <path>\n\`\`\`lang\n<body>\n\`\`\`` 形に。 -/
private def renderFile (relPath : String) (body : String) : String :=
  let lang := guessLang relPath
  s!"\n\n## File: {relPath}\n\n```{lang}\n{body}\n```\n"

/-- 既定の system prompt — 「全ファイルを俯瞰してから具体所見」 -/
def defaultReviewSystem : String :=
"あなたはシニアソフトウェアエンジニアです。これから複数のソースファ
イルが Markdown の `## File: <path>` ブロック群として与えられます。
全ファイルに目を通してから、以下の順で日本語でレビューしてください。

1. **コードベースの俯瞰**: 全体構造・責務分割・主要な依存関係
2. **整合性チェック**: 異なるファイル間で矛盾している規約・パターン
3. **設計上の弱点**: アーキ的に脆い場所 (神クラス / 循環依存 / 過剰
   抽象化 / 抽象漏れ etc.)
4. **具体的バグリスク**: 個別ファイル中の defect 候補 (file:line で)
5. **改善提案**: 優先順を付けて 3〜10 件

ファイル単独のスタイル指摘より、**ファイル間の繋がり / 設計の俯瞰**
を重視してください。"

structure ReviewStats where
  fileCount : Nat
  totalBytes : Nat
  /-- 読み込みでスキップしたファイル (理由付き)。 -/
  skipped : List (String × String) := []
  deriving Inhabited, Repr

/-- 複数ファイルを `SafePath` で workspace チェックしながら読み、
    Markdown に束ねて Gemini に渡す。

    - `workspace` は読み込み制限の root (`..` で外には出れない)。
    - `paths` は workspace からの相対パス。
    - `prompt` は user 質問。空の場合は「俯瞰レビューしてください」だけ。
    - `system` を指定しない場合は `defaultReviewSystem`。

    返り値: `Response` と、何ファイル何バイト送ったかの統計。 -/
def reviewMany (cfg : Config)
    (workspace : String)
    (paths : Array String)
    (prompt : String := "")
    (opts : CallOpts := {})
    : IO (Response × ReviewStats) := do
  let mut bundle := ""
  let mut count := 0
  let mut bytes := 0
  let mut skipped : List (String × String) := []
  for raw in paths do
    match SafePath.under workspace raw with
    | .error msg =>
      skipped := skipped ++ [(raw, s!"SafePath: {msg}")]
    | .ok p =>
      try
        let body ← IO.FS.readFile p.value
        bundle := bundle ++ renderFile raw body
        count := count + 1
        bytes := bytes + body.utf8ByteSize
      catch e =>
        skipped := skipped ++ [(raw, s!"read failed: {e}")]
  let userText :=
    (if prompt.isEmpty then defaultReviewSystem else prompt) ++ bundle
  let optsWithSys :=
    if opts.system.isSome then opts
    else { opts with system := some defaultReviewSystem }
  let resp ← ask cfg userText optsWithSys
  return (resp, { fileCount := count, totalBytes := bytes, skipped })

/-! ## Diff review — 軽量 git diff レビュー -/

/-- `git diff <base> <head>` を取って、それを Gemini にレビューさせる。 -/
def reviewDiff (cfg : Config) (workspace base head : String)
    (prompt : String := "") (opts : CallOpts := {})
    : IO Response := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["-C", workspace, "diff", base, head]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"git diff failed: {out.stderr}"
  let diff := out.stdout
  let sysDefault :=
"あなたはシニアソフトウェアエンジニアです。これから unified diff
が渡されます。以下の順で日本語でレビューしてください。

1. **意図**: この変更は何を達成しようとしているか
2. **欠陥候補**: バグ・regression リスク・抜け
3. **改善提案**: スタイル / 性能 / 保守性 で気になる点
4. **承認可否の総評**: approve / request changes / nit のどれか + 理由"
  let userText :=
    (if prompt.isEmpty then "上記のレビュー観点でお願いします。" else prompt)
    ++ "\n\n```diff\n" ++ diff ++ "\n```"
  let optsWithSys :=
    if opts.system.isSome then opts
    else { opts with system := some sysDefault }
  ask cfg userText optsWithSys

end LeanTea.Cloud.Gemini
