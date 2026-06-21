import LeanTea

open LeanTea

structure Model where
  count : Int

inductive Msg where
  | inc
  | dec
  | reset

def view (m : Model) : Html :=
  div_ [] [
    h1 [] [text "LeanTea Web Counter"],
    p [("class","muted")] [text "静的シェルは事前生成、状態は X-Model ヘッダで往復、サーバーはステートレス"],
    div_ [("class","card")] [
      p [] [text s!"count = {m.count}"],
      div_ [("class","row")] [
        a_ [("class","l primary"),("href","#"),("data-msg","inc")] [text "＋ inc"],
        a_ [("class","l"),("href","#"),("data-msg","dec")] [text "− dec"],
        a_ [("class","l ghost"),("href","#"),("data-msg","reset")] [text "↺ reset"]
      ]
    ]
  ]

def encodeModel (m : Model) : String := toString m.count

def decodeModel (s : String) : Option Model :=
  match s.toInt? with
  | some n => some { count := n }
  | none   => none

def decodeMsg : String → Option Msg
  | "inc"   => some .inc
  | "dec"   => some .dec
  | "reset" => some .reset
  | _       => none

def update : Msg → Model → Model
  | .inc,   m => { m with count := m.count + 1 }
  | .dec,   m => { m with count := m.count - 1 }
  | .reset, _ => { count := 0 }

def app : WebApp Model Msg :=
  { init := { count := 0 }
    title := "LeanTea Counter"
    update, view, encodeModel, decodeModel, decodeMsg }

def main (args : List String) : IO Unit := WebApp.run app args
