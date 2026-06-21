import LeanTea

/-! # gpu_serve — minimal WebGPU demo

Boots a fullscreen canvas with the stock fragment shader from
`LeanTea.WebGpu`. Useful as a sanity check for the framework's
WebGPU helper and as a starting point for shader sketches:
copy this file, swap the shader string, rebuild. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server

namespace GpuServe

def handler : Handler := fun req => do
  match req.path with
  | "/" =>
    let body := WebGpu.page "lean-elm" WebGpu.demoShader
    return Response.html 200 body
  | "/favicon.ico" =>
    return { status := 204, headers := #[], body := .empty }
  | _ => return Response.notFound

private structure Args where
  port : UInt16 := 8004
  host : String := "0.0.0.0"

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port" :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 8004).toUInt16 }
  | "--host" :: v :: rest => parseArgs rest { a with host := v }
  | _ :: rest             => parseArgs rest a
  | []                    => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some envPort ← IO.getEnv "PORT" then
    if let some n := envPort.toNat? then a := { a with port := n.toUInt16 }
  IO.println s!"gpu server: http://{a.host}:{a.port}/"
  serve a.port a.host handler

end GpuServe

def main (args : List String) : IO Unit := GpuServe.serveMain args
