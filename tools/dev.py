#!/usr/bin/env python3
"""dev.py — watch Lean source, rebuild and restart canvas_serve.

Usage:
    python3 tools/dev.py [--app canvas|english] [--port 8801]

Loops doing:

  1. lake build <app>_serve
  2. start the binary with DEV_MODE=1 so the HTML embeds a tiny
     poller that hits /_dev/ping every second
  3. watch *.lean under LeanTea/ and examples/ — on change, kill the
     current server, rebuild, restart. Browser auto-reloads via the
     ping poller (it sees a new startup timestamp).

Stdlib-only (no inotify / watchdog dep).
"""
from __future__ import annotations

import argparse
import os
import signal
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
LEAN_DIR = ROOT
STATE_DIR = ROOT.parent / ".leantea-state"
WATCH_GLOBS = [
    "LeanTea/**/*.lean",
    "LeanTea.lean",
    "examples/*.lean",
    "lakefile.lean",
]


def list_files() -> list[Path]:
    out: list[Path] = []
    for g in WATCH_GLOBS:
        out.extend(LEAN_DIR.glob(g))
    return [p for p in out if p.is_file()]


def fingerprint(files: list[Path]) -> dict[str, float]:
    return {str(p): p.stat().st_mtime for p in files}


def build(target: str) -> bool:
    print(f"[dev] lake build {target}", file=sys.stderr)
    r = subprocess.run(["lake", "build", target], cwd=LEAN_DIR)
    return r.returncode == 0


def start_server(app: str, port: int) -> subprocess.Popen:
    binary = LEAN_DIR / ".lake" / "build" / "bin" / f"{app}_serve"
    db = STATE_DIR / f"{app}-dev.sqlite"
    db.parent.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    env["DEV_MODE"] = "1"
    args = [str(binary), "--port", str(port), "--db", str(db)]
    if app == "english":
        args += ["--dist", str(ROOT / "dist" / "english")]
    print(f"[dev] starting {app}_serve on :{port}", file=sys.stderr)
    return subprocess.Popen(args, env=env)


def stop_server(proc: subprocess.Popen | None) -> None:
    if proc is None or proc.poll() is not None:
        return
    proc.terminate()
    try:
        proc.wait(timeout=3)
    except subprocess.TimeoutExpired:
        proc.kill()


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("--app", default="canvas", choices=["canvas", "english"])
    p.add_argument("--port", type=int, default=8801)
    args = p.parse_args()

    target = f"{args.app}_serve"
    fp = fingerprint(list_files())
    if not build(target):
        print("[dev] initial build failed", file=sys.stderr)
        sys.exit(1)
    proc = start_server(args.app, args.port)
    print(f"[dev] open http://127.0.0.1:{args.port}/", file=sys.stderr)
    print("[dev] watching for source changes... (Ctrl+C to stop)", file=sys.stderr)
    try:
        while True:
            time.sleep(0.4)
            new_fp = fingerprint(list_files())
            if new_fp != fp:
                changed = [k for k in set(new_fp) | set(fp)
                           if new_fp.get(k) != fp.get(k)]
                short = [Path(c).name for c in changed[:3]]
                print(f"[dev] change: {', '.join(short)}", file=sys.stderr)
                fp = new_fp
                if build(target):
                    stop_server(proc)
                    proc = start_server(args.app, args.port)
                else:
                    print("[dev] build failed — keeping previous server",
                          file=sys.stderr)
    except KeyboardInterrupt:
        print("\n[dev] stopping", file=sys.stderr)
    finally:
        stop_server(proc)


if __name__ == "__main__":
    main()
