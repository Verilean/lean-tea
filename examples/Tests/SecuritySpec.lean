import LeanTea
import LeanTea.Html.Safe
import LeanTea.Net.SafePath
import LeanTea.Net.SafeRedirect
import LeanTea.Net.Csp
import LeanTea.Os.SafeCmd
import LeanTea.Form.Csrf
import LeanTea.StateMachine
import StateMachine.Order

/-! # examples/Tests/SecuritySpec.lean — one binary for the
    construction-time security guarantees.

Consolidates the per-primitive smoke binaries (SafeHtml, SafePath,
SafeCmd, SafeHeader, SafeRedirect) into a single LSpec runner so CI
runs **one step** instead of five. The richer integration smokes
(`auth_proof_smoke`, `safequery_smoke`) keep their own binaries
because they spin up SQLite + sessions.

Each `group` mirrors one primitive's `SECURITY.md` section. Adding
a new allow-list entry to a primitive? Add an `it` here and the
build fails until the new case is green. -/

open LeanTea LeanTea.Html LeanTea.LSpec
open LeanTea.Net.Http (Response)
open LeanTea.Net (SafePath SafeRedirect Csp CspSrc)
open LeanTea.Net.Http (Response.csp)
open LeanTea.Os (SafeCmd)
open LeanTea.Form.Csrf (Config hiddenInput)

/-! ## helpers -/

private def isOk    [Inhabited α] : Except String α → Bool | .ok _ => true | .error _ => false
private def isError [Inhabited α] : Except String α → Bool | .ok _ => false | .error _ => true

private def hasSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-! ## 1 · SafeHtml — XSS via attribute names and URL schemes -/

def safeHtmlSpec : LSpec := group "SafeHtml (Primitive 4)" [
  group "rejected attribute names" [
    it "onclick rejected"                  (isError (SafeAttr.text "onclick" "x")),
    it "onerror rejected"                  (isError (SafeAttr.text "onerror" "x")),
    it "style (not on allow-list)"         (isError (SafeAttr.text "style"   "x"))
  ],
  group "rejected URL schemes" [
    it "javascript: rejected"              (isError (SafeAttr.url  "href" "javascript:alert(1)")),
    it "data:text/html rejected"           (isError (SafeAttr.url  "href" "data:text/html,1")),
    it "case-insensitive javascript:"      (isError (SafeAttr.url  "href" "JaVaScRiPt:1")),
    it "vbscript: rejected"                (isError (SafeAttr.url  "href" "vbscript:msgbox()"))
  ],
  group "accepted (allow-list)" [
    it "class accepted"                    (isOk    (SafeAttr.text "class" "btn")),
    it "data-* accepted"                   (isOk    (SafeAttr.text "data-test" "x")),
    it "aria-* accepted"                   (isOk    (SafeAttr.text "aria-label" "x")),
    it "relative URL accepted"             (isOk    (SafeAttr.url  "href" "/login")),
    it "https URL accepted"                (isOk    (SafeAttr.url  "href" "https://x.com")),
    it "mailto URL accepted"               (isOk    (SafeAttr.url  "href" "mailto:hi@x.com"))
  ]
]

/-! ## 2 · SafePath — Path Traversal -/

def safePathSpec : LSpec :=
  let ws := "/srv/uploads"
  let okAndEq (s : Except String SafePath) (expected : String) : Bool :=
    match s with | .ok p => p.value == expected | .error _ => false
  group "SafePath (Primitive 5)" [
    group "accepted" [
      it "relative joined"
        (okAndEq (SafePath.under ws "a.txt") "/srv/uploads/a.txt"),
      it "nested relative"
        (okAndEq (SafePath.under ws "sub/dir/b.txt") "/srv/uploads/sub/dir/b.txt"),
      it "absolute under ws"
        (okAndEq (SafePath.under ws "/srv/uploads/sub/x") "/srv/uploads/sub/x"),
      it "`.`/`..` normalised"
        (okAndEq (SafePath.under ws "a/./b/../c.txt") "/srv/uploads/a/c.txt"),
      it "trailing-slash workspace"
        (okAndEq (SafePath.under "/srv/uploads/" "ok.txt") "/srv/uploads/ok.txt")
    ],
    group "rejected" [
      it "`..` escapes"                      (isError (SafePath.under ws "../etc/passwd")),
      it "deep `..` escapes"                 (isError (SafePath.under ws "a/../../etc/passwd")),
      it "absolute outside ws"               (isError (SafePath.under ws "/etc/passwd")),
      it "sibling-prefix attack"             (isError (SafePath.under ws "/srv/uploads-attacker/x")),
      it "NUL byte"                          (isError (SafePath.under ws "a.txt\u0000.png"))
    ]
  ]

/-! ## 3 · SafeCmd — OS Command Injection -/

def safeCmdSpec : LSpec := group "SafeCmd (Primitive 6)" [
  group "accepted argv-style" [
    it "echo"            (isOk    (SafeCmd.exec "echo" ["hi"])),
    it "/usr/bin/env"    (isOk    (SafeCmd.exec "/usr/bin/env" ["printf", "x"])),
    it "ls"              (isOk    (SafeCmd.exec "ls" ["-la"]))
  ],
  group "rejected shells by basename" [
    it "sh"              (isError (SafeCmd.exec "sh" ["-c", "evil"])),
    it "bash"            (isError (SafeCmd.exec "bash" ["-c", "evil"])),
    it "/usr/bin/bash"   (isError (SafeCmd.exec "/usr/bin/bash" ["-c", "evil"])),
    it "/bin/zsh"        (isError (SafeCmd.exec "/bin/zsh" ["-c", "evil"]))
  ],
  group "rejected NUL bytes" [
    it "NUL in cmd"      (isError (SafeCmd.exec "echo\u0000sh" ["hi"])),
    it "NUL in args"     (isError (SafeCmd.exec "echo" ["hi\u0000; rm"]))
  ]
]

/-! ## 4 · SafeHeader — HTTP header injection + clickjacking baseline -/

def safeHeaderSpec : LSpec :=
  let r0 : Response := .text 200 "ok"
  let serialised (r : Response) : String := String.fromUTF8! r.toBytes
  let defaultHas (needle : String) : Bool :=
    hasSubstr (serialised r0.defaultSecurityHeaders) needle
  let optedOut := r0.defaultSecurityHeaders (frameOptions := none)
  let optedOutS := serialised optedOut
  let traceBytes := match r0.setHeader "x-trace-id" "abc-123" with
                    | .ok r    => serialised r
                    | .error _ => ""
  let redirSerialised := serialised (Response.redirect "/dash\r\nset-cookie: pwned=1")
  group "Response.setHeader + defaultSecurityHeaders (Primitive 7)" [
    group "CRLF/NUL rejected" [
      it "CRLF in name"        (isError (r0.setHeader "x-evil\r\nset-cookie" "x")),
      it "CRLF in value"       (isError (r0.setHeader "x-test" "evil\r\nset-cookie: a=b")),
      it "LF-only in value"    (isError (r0.setHeader "x-test" "evil\nset-cookie: a=b")),
      it "NUL in value"        (isError (r0.setHeader "x-test" "ok\u0000x"))
    ],
    group "safe header lives through serialisation" [
      it "x-trace-id appears in toBytes" (hasSubstr traceBytes "x-trace-id: abc-123")
    ],
    group "defaultSecurityHeaders" [
      it "X-Frame-Options: DENY"           (defaultHas "x-frame-options: DENY"),
      it "X-Content-Type-Options: nosniff" (defaultHas "x-content-type-options: nosniff"),
      it "Referrer-Policy: no-referrer"    (defaultHas "referrer-policy: no-referrer"),
      it "Permissions-Policy: …"           (defaultHas "permissions-policy: geolocation=()")
    ],
    group "frameOptions := none opts out" [
      it "X-Frame-Options absent" (!hasSubstr optedOutS "x-frame-options"),
      it "but nosniff still present" (hasSubstr optedOutS "x-content-type-options: nosniff")
    ],
    group "Response.redirect strips CR/LF (defence in depth)" [
      it "no injected `\\r\\nset-cookie:`"  (!hasSubstr redirSerialised "\r\nset-cookie: pwned"),
      it "location header still emitted"   (hasSubstr redirSerialised "location: /dash")
    ]
  ]

/-! ## 5 · SafeRedirect — Open Redirect -/

def safeRedirectSpec : LSpec :=
  let trusted := ["https://app.example", "https://api.example.com"]
  let okAndEq (s : Except String SafeRedirect) (expected : String) : Bool :=
    match s with | .ok r => r.location == expected | .error _ => false
  group "SafeRedirect (Primitive 8)" [
    group "accepted relative paths" [
      it "/dashboard"     (okAndEq (SafeRedirect.to trusted "/dashboard") "/dashboard"),
      it "path+query"     (okAndEq (SafeRedirect.to trusted "/users/me?x=1") "/users/me?x=1")
    ],
    group "accepted trusted origins" [
      it "exact origin"        (okAndEq (SafeRedirect.to trusted "https://app.example") "https://app.example"),
      it "path under origin"   (isOk    (SafeRedirect.to trusted "https://app.example/foo/bar"))
    ],
    group "rejected protocol-relative" [
      it "//evil.example"      (isError (SafeRedirect.to trusted "//evil.example/x")),
      it "/\\\\evil.example"   (isError (SafeRedirect.to trusted "/\\evil.example/x"))
    ],
    group "rejected dangerous schemes" [
      it "javascript:"             (isError (SafeRedirect.to trusted "javascript:alert(1)")),
      it "javascript: case-insens" (isError (SafeRedirect.to trusted "JaVaScRiPt:1")),
      it "data:"                   (isError (SafeRedirect.to trusted "data:text/html,1")),
      it "vbscript:"               (isError (SafeRedirect.to trusted "vbscript:1")),
      it "file:"                   (isError (SafeRedirect.to trusted "file:///etc/passwd"))
    ],
    group "rejected: not on allow-list" [
      it "evil origin"             (isError (SafeRedirect.to trusted "https://evil.example/x")),
      it "sibling-prefix origin"   (isError (SafeRedirect.to trusted "https://app.example.evil.com/x"))
    ],
    group "rejected: CR/LF/NUL (defence in depth)" [
      it "CRLF"                    (isError (SafeRedirect.to trusted "/foo\r\nset-cookie: pwned")),
      it "NUL"                     (isError (SafeRedirect.to trusted "/foo\u0000bar"))
    ]
  ]

/-! ## 6 · CSP typed builder -/

def cspSpec : LSpec :=
  let strict := Csp.strict.render
  let custom : Csp :=
    { Csp.strict with
      scriptSrc := [.self, .host "https://cdn.example.com", .nonce "abc123"],
      reportTo  := some "csp-endpoint" }
  let customRendered := custom.render
  let reportOnly : Csp := { Csp.strict with reportOnly := true }
  group "Net.Csp (Primitive 9)" [
    group "render emits the directive name and source keywords" [
      it "strict has default-src 'none'"      (hasSubstr strict "default-src 'none'"),
      it "strict has script-src 'self'"       (hasSubstr strict "script-src 'self'"),
      it "strict has frame-ancestors 'none'"  (hasSubstr strict "frame-ancestors 'none'"),
      it "directive separator is `; `"        (hasSubstr strict "; ")
    ],
    group "custom directives are typed values, not strings" [
      it "host source rendered verbatim"
        (hasSubstr customRendered "https://cdn.example.com"),
      it "nonce wrapped in single quotes"
        (hasSubstr customRendered "'nonce-abc123'"),
      it "report-to directive present"
        (hasSubstr customRendered "report-to csp-endpoint")
    ],
    group "reportOnly flips the header *name*" [
      it "Csp.strict → enforce header"
        (Csp.strict.headerName == "content-security-policy"),
      it "reportOnly → report-only header"
        (reportOnly.headerName == "content-security-policy-report-only")
    ],
    group "Response.csp attaches via setHeader! (CR/LF still refused)" [
      it "header lands in toBytes" (
        let resp : Response := (Response.text 200 "ok").csp Csp.strict
        let s := String.fromUTF8! resp.toBytes
        hasSubstr s "content-security-policy: default-src 'none'")
    ]
  ]

/-! ## 7 · CSRF double-submit token -/

def csrfSpec : LSpec :=
  let cfg : LeanTea.Form.Csrf.Config := { secure := false }
  let mkReq (cookie body : String) : LeanTea.Net.Http.Request := {
    method  := "POST", path := "/save", query := "",
    headers := if cookie.isEmpty then #[] else #[("cookie", cookie)],
    body    := body.toUTF8 }
  let goodReq     := mkReq "csrf=abc123" "_csrf=abc123&other=field"
  let mismatchReq := mkReq "csrf=abc123" "_csrf=DIFFERENT&other=field"
  let noCookieReq := mkReq ""            "_csrf=abc123"
  let noFieldReq  := mkReq "csrf=abc123" "other=field"
  let html := (LeanTea.Form.Csrf.hiddenInput cfg "abc123").render
  let okOf (e : Except String Unit) : Bool :=
    match e with | .ok ()    => true  | .error _ => false
  let errOf (e : Except String Unit) : Bool :=
    match e with | .ok ()    => false | .error _ => true
  group "Form.Csrf (Primitive 10)" [
    group "verify accepts matching cookie + field" [
      it "double-submit matches"  (okOf  (cfg.verify goodReq))
    ],
    group "verify refuses tampering" [
      it "mismatched values"      (errOf (cfg.verify mismatchReq)),
      it "missing cookie"         (errOf (cfg.verify noCookieReq)),
      it "missing field"          (errOf (cfg.verify noFieldReq))
    ],
    group "hidden input renders as expected" [
      it "name attribute"   (hasSubstr html "name=\"_csrf\""),
      it "value attribute"  (hasSubstr html "value=\"abc123\""),
      it "type=hidden"      (hasSubstr html "type=\"hidden\"")
    ]
  ]

/-! ## 8 · State-machine GADT (Order example) -/

def stateMachineSpec : LSpec :=
  /- Build the legal lifecycle. Each `apply` is a separate compile
     step — the types stitch through `draft → submitted → paid →
     shipped` exactly because the constructors line up. -/
  let order0  : OrderSM.Order .draft     := OrderSM.fresh 1 1000
  let order1  : OrderSM.Order .submitted := order0.apply .submit
  let order2  : OrderSM.Order .paid      := order1.apply .pay
  let order3  : OrderSM.Order .shipped   := order2.apply .ship
  let order2c : OrderSM.Order .cancelled := order2.apply .cancel
  group "StateMachine (Primitive 11) — Order lifecycle" [
    group "legal transitions compile and update the payload" [
      it "draft.submit preserves total"
        (order1.id == 1 && order1.total == 1000),
      it "submitted.pay fills paidAmt"
        (order2.paidAmt == 1000),
      it "paid.ship sets shippedAt"
        (order3.shippedAt == 1_700_000_000),
      it "any.cancel zeroes payload"
        (order2c.total == 0 && order2c.paidAmt == 0)
    ],
    group "illegal transitions are documented in source as compile errors" [
      /-
        Try uncommenting:

          let bad := order0.apply OrderSM.Transition.pay
          -- error: application type mismatch
          --   expected OrderTransition .draft ?
          --   got      OrderTransition .submitted .paid

        and the build refuses to proceed. There is **no value-level
        path** from a `.draft` order to `.paid` — Lean's dependent
        types make the bug class unrepresentable.
      -/
      it "(see source comment block — the compile error is the test)" true
    ]
  ]

/-! ## Aggregate tree + entry point -/

def allSpecs : LSpec := group "LeanTEA construction-time security" [
  safeHtmlSpec,
  safePathSpec,
  safeCmdSpec,
  safeHeaderSpec,
  safeRedirectSpec,
  cspSpec,
  csrfSpec,
  stateMachineSpec
]

def main : IO Unit := do
  let code ← lspecIO allSpecs
  if code != 0 then IO.Process.exit code.toUInt8
