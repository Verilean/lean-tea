/-! # LeanTea.WebGpu — boot a WebGPU canvas from one WGSL fragment

A shadertoy-shaped helper. The framework provides:

* A full-screen triangle vertex shader (so the user only writes
  the fragment shader)
* A standard uniform buffer with `(width, height, time)` so
  shaders can react to size and animate
* A render loop pinned to `requestAnimationFrame`
* A no-WebGPU fallback message so the page degrades visibly

The user provides:

* The WGSL fragment shader source (one `@fragment` function returning
  `vec4f`)
* Optionally, the `<canvas>` size — defaults to fullscreen

Usage from an app:

```lean
import LeanTea

def shader : String := "
@fragment fn fs(@builtin(position) p: vec4f) -> @location(0) vec4f {
  let uv = p.xy / u.resolution;
  let c  = 0.5 + 0.5 * cos(u.time + uv.xyx + vec3f(0., 2., 4.));
  return vec4f(c, 1.0);
}"

def homePage : Response :=
  Response.html 200 (LeanTea.WebGpu.page "Hello WebGPU" shader)
```

Browser requirements: Chromium 113+, Safari 18+, Firefox 141+. -/

namespace LeanTea.WebGpu

/-- Stock vertex shader: emits a full-screen triangle that covers
    the viewport so the fragment shader runs once per pixel. -/
def stockVertex : String :=
  "@vertex fn vs(@builtin(vertex_index) i : u32) -> @builtin(position) vec4f {\n"
  ++ "  let p = array<vec2f, 3>(vec2f(-1.,-1.), vec2f(3.,-1.), vec2f(-1.,3.));\n"
  ++ "  return vec4f(p[i], 0., 1.);\n"
  ++ "}"

/-- Uniform block bound at @group(0) @binding(0). Shaders refer to
    fields as `u.resolution`, `u.time`. -/
def uniformBlock : String :=
  "struct U { resolution: vec2f, time: f32, _pad: f32 };\n"
  ++ "@group(0) @binding(0) var<uniform> u : U;"

/-- Build the WGSL source the GPU sees: uniforms + vertex + the
    user-supplied fragment, in order. -/
def wgsl (fragment : String) : String :=
  uniformBlock ++ "\n\n" ++ stockVertex ++ "\n\n" ++ fragment

/-- Module script that initialises WebGPU, compiles the shader,
    creates the pipeline, allocates the uniform buffer and starts
    the render loop. Emits a graceful `<noscript>`-style message
    if `navigator.gpu` is missing. -/
def bootJs (canvasId : String) (fullSource : String) : String :=
  "(async () => {\n"
  ++ s!"  const canvas = document.getElementById('{canvasId}');\n"
  ++ "  if (!navigator.gpu) {\n"
  ++ "    canvas.outerHTML = '<div style=\"color:#fb923c;font-family:monospace;padding:24px\">"
  ++ "WebGPU not available in this browser. Try Chrome 113+, Safari 18+, or Firefox 141+.</div>';\n"
  ++ "    return;\n"
  ++ "  }\n"
  ++ "  const adapter = await navigator.gpu.requestAdapter();\n"
  ++ "  if (!adapter) { canvas.outerHTML = '<div>No GPU adapter</div>'; return; }\n"
  ++ "  const device = await adapter.requestDevice();\n"
  ++ "  const ctx = canvas.getContext('webgpu');\n"
  ++ "  const fmt = navigator.gpu.getPreferredCanvasFormat();\n"
  ++ "  ctx.configure({ device, format: fmt, alphaMode: 'premultiplied' });\n"
  ++ "  // 16 bytes: vec2f resolution + f32 time + f32 pad\n"
  ++ "  const uBuf = device.createBuffer({ size: 16, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });\n"
  ++ "  const module = device.createShaderModule({ code: "
  ++ String.quote fullSource
  ++ " });\n"
  ++ "  const pipeline = device.createRenderPipeline({\n"
  ++ "    layout: 'auto',\n"
  ++ "    vertex:   { module, entryPoint: 'vs' },\n"
  ++ "    fragment: { module, entryPoint: 'fs', targets: [{ format: fmt }] },\n"
  ++ "    primitive: { topology: 'triangle-list' }\n"
  ++ "  });\n"
  ++ "  const bindGroup = device.createBindGroup({\n"
  ++ "    layout: pipeline.getBindGroupLayout(0),\n"
  ++ "    entries: [{ binding: 0, resource: { buffer: uBuf } }]\n"
  ++ "  });\n"
  ++ "  function resize(){\n"
  ++ "    const dpr = window.devicePixelRatio || 1;\n"
  ++ "    const w = Math.max(1, Math.floor(canvas.clientWidth  * dpr));\n"
  ++ "    const h = Math.max(1, Math.floor(canvas.clientHeight * dpr));\n"
  ++ "    if (canvas.width !== w || canvas.height !== h) { canvas.width = w; canvas.height = h; }\n"
  ++ "  }\n"
  ++ "  window.addEventListener('resize', resize); resize();\n"
  ++ "  const t0 = performance.now();\n"
  ++ "  function frame(){\n"
  ++ "    resize();\n"
  ++ "    const t = (performance.now() - t0) / 1000;\n"
  ++ "    device.queue.writeBuffer(uBuf, 0,\n"
  ++ "      new Float32Array([canvas.width, canvas.height, t, 0]));\n"
  ++ "    const enc = device.createCommandEncoder();\n"
  ++ "    const pass = enc.beginRenderPass({\n"
  ++ "      colorAttachments: [{\n"
  ++ "        view: ctx.getCurrentTexture().createView(),\n"
  ++ "        loadOp: 'clear', storeOp: 'store',\n"
  ++ "        clearValue: { r: 0, g: 0, b: 0, a: 1 }\n"
  ++ "      }]\n"
  ++ "    });\n"
  ++ "    pass.setPipeline(pipeline);\n"
  ++ "    pass.setBindGroup(0, bindGroup);\n"
  ++ "    pass.draw(3);\n"
  ++ "    pass.end();\n"
  ++ "    device.queue.submit([enc.finish()]);\n"
  ++ "    requestAnimationFrame(frame);\n"
  ++ "  }\n"
  ++ "  requestAnimationFrame(frame);\n"
  ++ "})();"

/-- Full HTML page rendering `fragment` (a WGSL `@fragment` function)
    on a fullscreen canvas. Title goes in `<title>` and as a tiny
    badge in the top-left so the source app is identifiable. -/
def page (title : String) (fragment : String) (canvasId : String := "gpu")
    : String :=
  let source := wgsl fragment
  "<!DOCTYPE html>\n<html lang=\"en\"><head>\n"
  ++ "<meta charset=\"UTF-8\">\n"
  ++ s!"<title>{title}</title>\n"
  ++ "<style>html,body{margin:0;height:100%;background:#000;overflow:hidden}"
  ++ "canvas{display:block;width:100%;height:100%}"
  ++ ".badge{position:fixed;top:8px;left:10px;color:#94a3b8;"
  ++ "font:11px/1.2 'Segoe UI',monospace;background:rgba(0,0,0,.4);"
  ++ "padding:4px 8px;border-radius:4px;pointer-events:none}</style>\n"
  ++ "</head><body>\n"
  ++ s!"<canvas id=\"{canvasId}\"></canvas>\n"
  ++ s!"<div class=\"badge\">{title} · WebGPU</div>\n"
  ++ "<script type=\"module\">\n" ++ bootJs canvasId source ++ "\n</script>\n"
  ++ "</body></html>"

/-- A built-in shader so the user can ship a working WebGPU demo
    without writing any WGSL themselves. -/
def demoShader : String :=
  "@fragment fn fs(@builtin(position) p: vec4f) -> @location(0) vec4f {\n"
  ++ "  let uv = p.xy / u.resolution;\n"
  ++ "  let c  = 0.5 + 0.5 * cos(u.time + uv.xyx + vec3f(0., 2., 4.));\n"
  ++ "  return vec4f(c, 1.0);\n"
  ++ "}"

end LeanTea.WebGpu
