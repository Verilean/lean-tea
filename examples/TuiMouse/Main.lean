import LeanTea.Tui

/-! # tui_mouse — mouse, resize and wide characters in `LeanTea.Tui`

```
$ lake exe tui_mouse
```

  * drag the vertical divider (between the file list and the right
    side) or the horizontal one (between the log and the buttons)
  * click a file to select it, wheel over the list to move
  * wheel over the log, or press / drag its scrollbar
  * click the buttons; Tab still cycles focus, Enter activates
  * resize the terminal window — the layout follows
  * Ctrl+C or the `[終了]` button quits

All state (split ratios, scroll offset, selection) lives in `State`;
the widgets only report changes, as everywhere else in the kit. -/

open LeanTea.Tui

namespace TuiMouse

inductive Msg where
  | ratio (r : Nat) | vratio (r : Nat) | scrolled (o : Nat)
  | up | down | pick (i : Nat) | openSel
  | inc | dec | clear | quit

structure State where
  ratio  : Nat := 350
  vratio : Nat := 700
  offset : Nat := 0
  sel    : Nat := 0
  count  : Int := 0
  log    : Array String := (List.range 30).toArray.map (s!"起動ログ {·}: 古い行はホイールでスクロール")
  quit   : Bool := false

def files : List String :=
  ["README.md", "設計メモ.md", "lakefile.lean", "Main.lean", "テスト結果.txt", "画像_01.png",
   "画像_02.png", "data.csv", "ノート.org", "LICENSE", "変更履歴.md", "build.sh"]

def note (s : State) (line : String) : State :=
  let log := s.log.push line
  -- follow the tail: keep the newest line in view
  { s with log, offset := log.size }

def update (msg : Msg) (s : State) : State :=
  match msg with
  | .ratio r    => { s with ratio := r }
  | .vratio r   => { s with vratio := r }
  | .scrolled o => { s with offset := o }
  | .up         => { s with sel := s.sel - 1 }
  | .down       => { s with sel := min (files.length - 1) (s.sel + 1) }
  | .pick i     => note { s with sel := i } s!"選択: {files.getD i "?"}"
  | .openSel    => note s s!"開く: {files.getD s.sel "?"}"
  | .inc        => note { s with count := s.count + 1 } s!"＋ → {s.count + 1}"
  | .dec        => note { s with count := s.count - 1 } s!"－ → {s.count - 1}"
  | .clear      => { s with log := #[], offset := 0 }
  | .quit       => { s with quit := true }

def view (s : State) : Widget Msg :=
  let logLines : List (Widget Msg) := s.log.toList.map (text ·)
  vbox [
    text " LeanTea.Tui マウスデモ — クリック・ドラッグ・ホイール・リサイズ (Ctrl+C で終了)"
      { fg := .black, bg := .cyan, bold := true },
    hsplit s.ratio .ratio
      (panel "ファイル" .line (listView "files" files s.sel .up .down .openSel (onPick := some .pick)))
      (vsplit s.vratio .vratio
        (panel "ログ" .line (scroll s.offset .scrolled (vbox logLines) (contentHeight := some s.log.size)))
        (panel "操作" .line (vbox [
          text s!"カウント: {s.count}",
          hbox [
            button "inc" .primary "[＋]" .inc,
            button "dec" .secondary "[－]" .dec,
            button "clear" .ghost "[ログ消去]" .clear,
            button "quit" .danger "[終了]" .quit
          ]
        ]))),
    text s!" 分割 {s.ratio / 10}% / {s.vratio / 10}%  ログ {s.log.size} 行  選択 {files.getD s.sel ""}"
      { dim := true }
  ]

def app : App State Msg := {
  init := {},
  view,
  update,
  quitWhen := (·.quit),
  initialFocus := "files"
}

end TuiMouse

def main : IO Unit := TuiMouse.app.run
