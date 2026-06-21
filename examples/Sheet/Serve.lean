import LeanTea
import Sheet.App

/-! # sheet_serve — HTTP server for the functional spreadsheet.

Same shape as `counter_web`: serves a static HTML shell at `/`,
swaps in the runtime + initial model on first paint, then handles
`/api/step` for each `Msg`. The model rides in `X-Model`. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server

namespace SheetServe

private def styles : String :=
"body { background:#0f172a; color:#e2e8f0; font-family:'Segoe UI',sans-serif; margin:0; padding:24px; }
.sheet-app { max-width: 980px; margin: 0 auto; }
.sheet-toolbar { display:flex; gap:8px; align-items:center; margin-bottom:12px; background:#1e293b; padding:10px 14px; border-radius:8px; }
.cell-name { font-weight:700; font-family:monospace; color:#38bdf8; min-width:42px; }
.formula-form { display:flex; gap:6px; flex:1; }
.formula-input { flex:1; padding:6px 10px; border-radius:6px; border:1px solid #334155; background:#0f172a; color:#e2e8f0; font-family:monospace; font-size:0.9rem; }
.formula-input:focus { outline:none; border-color:#38bdf8; }
.l { display:inline-block; padding:6px 14px; border-radius:6px; cursor:pointer; text-decoration:none; font-size:0.85rem; font-weight:600; border:none; font-family:inherit; }
.l.primary { background:#0284c7; color:#fff; }
.l.primary:hover { background:#0369a1; }
.l.ghost { background:transparent; color:#94a3b8; border:1px solid #334155; }
.l.ghost:hover { color:#e2e8f0; border-color:#475569; }
.sheet-grid { border-collapse:collapse; width:100%; background:#1e293b; border-radius:8px; overflow:hidden; }
.sheet-grid th { background:#0f172a; color:#64748b; font-weight:600; font-size:0.75rem; padding:6px; border:1px solid #334155; }
.sheet-grid .corner { background:#0f172a; border:1px solid #334155; width:24px; }
.sheet-grid td.cell { border:1px solid #334155; padding:0; width:88px; height:36px; vertical-align:top; }
.sheet-grid td.cell.selected { box-shadow:inset 0 0 0 2px #38bdf8; }
.cell-anchor { display:flex; flex-direction:column; padding:3px 6px; text-decoration:none; color:inherit; height:100%; box-sizing:border-box; }
.cell-ref { font-family:monospace; font-size:0.62rem; color:#475569; }
.cell-value { font-family:monospace; font-size:0.85rem; color:#e2e8f0; margin-top:2px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.cell-value.error { color:#f87171; font-size:0.72rem; }
h1 { font-size:1.4rem; margin:0 0 18px; color:#38bdf8; }
.subtitle { color:#94a3b8; font-size:0.85rem; margin-bottom:14px; }
"

/-- The page shell. The TEA runtime swaps `#app`'s `innerHTML` and the
    `X-Model` header on every step. -/
private def indexHtml (initialBody initialModel : String) : String :=
  "<!DOCTYPE html>\n" ++
  "<html lang=\"ja\">\n<head>\n" ++
  "<meta charset=\"UTF-8\">\n" ++
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n" ++
  "<title>LeanTea Sheet</title>\n" ++
  s!"<style>{styles}</style>\n" ++
  "</head>\n<body>\n" ++
  "<h1>LeanTea Sheet</h1>\n" ++
  "<p class=\"subtitle\">Functional spreadsheet — cell formulas are LeanJs expressions, evaluated by the framework's built-in <code>LeanJs.Eval</code>. Try <code>42</code>, <code>A1 + B1 * 2</code>, or <code>if A1 == 0 then \"zero\" else \"non-zero\"</code>.</p>\n" ++
  s!"<div id=\"app\" data-model=\"{initialModel}\">{initialBody}</div>\n" ++
  "<script src=\"/runtime.js\"></script>\n" ++
  "</body>\n</html>\n"

/-! ## Handler. -/

def handler (storeRef : IO.Ref Sheet.Model) (req : Request) : IO Response := do
  match req.path with
  | "/" =>
    let m ← storeRef.get
    let (encoded, body) := WebApp.step Sheet.app (some (Sheet.app.encodeModel m)) none
    return Response.html 200 (indexHtml body encoded)
  | "/runtime.js" =>
    return {
      status := 200,
      headers := #[("content-type", "application/javascript"), ("cache-control", "no-store")],
      body := WebApp.runtimeJs.toUTF8
    }
  | "/api/step" =>
    let oldEnc := req.header? "x-model" |>.getD ""
    let msgRaw := LeanTea.Rpc.lookupParam req.query "msg" |>.getD ""
    /- POST body for form submits comes through as `ref=…&formula=…`. -/
    let msg :=
      if msgRaw == "set" then
        let body := match String.fromUTF8? req.body with | some s => s | none => ""
        "set:" ++ body
      else if msgRaw.isEmpty then ""
      else msgRaw
    let (encoded, html) := WebApp.step Sheet.app (some oldEnc) (some msg)
    /- Mirror the in-process store. -/
    match Sheet.app.decodeModel encoded with
    | some m => storeRef.set m
    | none   => pure ()
    return {
      status := 200,
      headers := #[("content-type", "text/html"),
                   ("cache-control", "no-store"),
                   ("x-model", encoded)],
      body := html.toUTF8
    }
  | _ => return Response.notFound

private structure Args where
  port : UInt16 := 8003
  host : String := "0.0.0.0"

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port" :: v :: rest =>
    parseArgs rest { a with port := (v.toNat?.getD 8003).toUInt16 }
  | "--host" :: v :: rest => parseArgs rest { a with host := v }
  | _ :: rest => parseArgs rest a
  | []        => a

def serveMain (args : List String) : IO Unit := do
  let a := parseArgs args {}
  let storeRef ← IO.mkRef Sheet.app.init
  IO.println s!"sheet_serve: http://{a.host}:{a.port}/"
  serve a.port a.host (handler storeRef)

end SheetServe

def main (args : List String) : IO Unit := SheetServe.serveMain args
