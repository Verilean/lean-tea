// Text on a screenshot, with boxes, from macOS's Vision framework.
//
//   leantea-ocr shot.png [lang ...]   (default languages: ja-JP en-US)
//
// Prints a JSON array of {"text", "conf", "x", "y", "w", "h"} in image
// pixels (origin top-left). Built on first use by LeanTea.Vision.Ocr.
import AppKit
import Foundation
import Vision

let args = CommandLine.arguments
guard args.count >= 2,
      let img = NSImage(contentsOfFile: args[1]),
      let cg = img.cgImage(forProposedRect: nil, context: nil, hints: nil) else {
  FileHandle.standardError.write("usage: leantea-ocr IMAGE.png [lang ...]\n".data(using: .utf8)!)
  exit(2)
}
let w = Double(cg.width), h = Double(cg.height)
let req = VNRecognizeTextRequest()
req.recognitionLevel = .accurate
req.recognitionLanguages = args.count > 2 ? Array(args[2...]) : ["ja-JP", "en-US"]
req.usesLanguageCorrection = false
try VNImageRequestHandler(cgImage: cg).perform([req])
var out: [[String: Any]] = []
for o in req.results ?? [] {
  guard let c = o.topCandidates(1).first else { continue }
  let b = o.boundingBox  // normalised, origin bottom-left
  out.append(["text": c.string, "conf": Double(c.confidence),
              "x": Int(b.minX * w), "y": Int((1 - b.maxY) * h),
              "w": Int(b.width * w), "h": Int(b.height * h)])
}
print(String(data: try JSONSerialization.data(withJSONObject: out), encoding: .utf8)!)
