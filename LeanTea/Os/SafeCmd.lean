/-! # LeanTea.Os.SafeCmd — process invocation that can't get shell-injected

Closes **OS Command Injection** (IPA 「安全なウェブサイトの作り方」§3.3,
OWASP A03, shadan-kun "OS コマンドインジェクション").

The Lean stdlib's `IO.Process.run` and `IO.Process.output` already
take `args : Array String` and call `execvp` directly — no shell.
That means a Lean program that *only* uses `Array String` arguments
is structurally immune to `; rm -rf /` style attacks.

The danger is one ergonomic step away:

```
-- THIS is what we want to make hard to write:
IO.Process.run { cmd := "sh", args := #["-c", "convert " ++ userInput] }
```

`SafeCmd` makes the safe path the easy path. The constructor is
`private mk`, the `args` field is `List String` (never concatenated),
and the only smart constructor `SafeCmd.exec` refuses programs whose
names look like a shell (`sh`, `bash`, `zsh`, `pwsh`, `cmd`) — those
need the audit-escape `SafeCmd.shell` which is grep-able. -/

namespace LeanTea.Os

/-- A validated process spawn: `cmd` + a list of pre-split `args`.
    Construct with `SafeCmd.exec` (allow-listed program names) or
    `SafeCmd.shell` (the grep-able audit escape). -/
structure SafeCmd where
  private mk ::
  cmd  : String
  args : List String
  deriving Inhabited, Repr

namespace SafeCmd

/-- Names that imply "this argv is going to be re-parsed by a shell".
    Reject by default — see `SafeCmd.shell` if you really want them. -/
private def shellNames : List String :=
  ["sh", "bash", "zsh", "dash", "ksh", "csh", "tcsh", "fish",
   "pwsh", "powershell", "cmd", "cmd.exe"]

/-- Reject NUL bytes (truncate paths/args at libc level) and reject
    program names that imply a re-parsing shell. -/
def exec (cmd : String) (args : List String) : Except String SafeCmd :=
  if cmd.contains '\u0000' then
    .error "SafeCmd.exec: NUL byte in cmd"
  else if args.any (·.contains '\u0000') then
    .error "SafeCmd.exec: NUL byte in args"
  else
    let bare := match (cmd.splitOn "/").reverse with
                | last :: _ => last
                | []        => cmd
    if shellNames.contains bare then
      .error s!"SafeCmd.exec: '{cmd}' is a shell — use SafeCmd.shell if you actually need this"
    else
      .ok ⟨cmd, args⟩

/-- Panic variant for literal commands in trusted code. Do **not**
    pass user input here. -/
def exec! (cmd : String) (args : List String) : SafeCmd :=
  match exec cmd args with
  | .ok c    => c
  | .error e => panic! s!"SafeCmd.exec!: {e}"

/-- **Audit-escape**: spawn a shell with a literal command string.
    Grep for `SafeCmd.shell` to find every place that intentionally
    bypasses the no-shell rule. The script is the *only* argv after
    `sh -c`, so user input concatenated into `script` is still RCE —
    callers must build `script` from constants only. -/
def shell (script : String) : SafeCmd := ⟨"sh", ["-c", script]⟩

/-- Drive `IO.Process.run` from a SafeCmd. Returns the exit code. -/
def run (s : SafeCmd) (cwd : Option System.FilePath := none) : IO UInt32 :=
  IO.Process.run { cmd := s.cmd, args := s.args.toArray, cwd := cwd } *> pure 0

/-- Drive `IO.Process.output` from a SafeCmd. Returns exit + stdout +
    stderr. -/
def output (s : SafeCmd) (cwd : Option System.FilePath := none) : IO IO.Process.Output :=
  IO.Process.output { cmd := s.cmd, args := s.args.toArray, cwd := cwd }

end SafeCmd

end LeanTea.Os
