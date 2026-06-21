#!/usr/bin/env python3
"""Background removal sidecar.

Tries `rembg` (ML, ~U2Net) first because it picks up complex
non-white backgrounds (e.g. FLUX's drifted pinkish gradients);
falls back to numpy-accelerated white-key thresholding otherwise.
That fallback handles clean white-bg sprites in ~30 ms.

Usage:  remove_bg.py input.png output.png [--threshold 235]

Exits non-zero on failure.
"""

import sys
import argparse


def via_rembg(inp: str, out: str) -> str:
    from rembg import remove
    data = open(inp, 'rb').read()
    open(out, 'wb').write(remove(data))
    return 'rembg'


def via_white_key(inp: str, out: str, threshold: int) -> str:
    import numpy as np
    from PIL import Image
    im = Image.open(inp).convert('RGBA')
    arr = np.array(im)
    # White-ish if all three RGB channels are above the threshold.
    white = (
        (arr[:, :, 0] >= threshold) &
        (arr[:, :, 1] >= threshold) &
        (arr[:, :, 2] >= threshold)
    )
    arr[white, 3] = 0
    Image.fromarray(arr).save(out)
    return 'pil-white-key'


def via_flood_key(inp: str, out: str, tol: int) -> str:
    """Flood-fill from the four image edges, marking every pixel
    reachable via colours within `tol` of the seed (= edge median)
    as transparent. Pixels enclosed by the foreground (eye whites,
    teeth, background-coloured patches inside the character) are
    untouched because the flood never reaches them.

    Robust against backgrounds that aren't pure white — Flux often
    produces pinkish or pale-blue gradients that pure threshold
    misclassifies. The seed colour is sampled per-side and we flood
    from every edge pixel, so a non-uniform background is still
    captured as long as it stays roughly contiguous.
    """
    import numpy as np
    from PIL import Image
    from collections import deque

    im = Image.open(inp).convert('RGBA')
    arr = np.array(im)
    h, w = arr.shape[:2]
    rgb = arr[:, :, :3].astype(np.int16)

    visited = np.zeros((h, w), dtype=bool)
    # Seed BFS with every border pixel.
    q = deque()
    for x in range(w):
        q.append((0, x)); q.append((h - 1, x))
    for y in range(h):
        q.append((y, 0)); q.append((y, w - 1))
    for y, x in q:
        visited[y, x] = True

    # 4-connected BFS. Compare each candidate to its parent: this
    # tolerates smooth gradients (every step is within `tol` even
    # if the corners are far apart in colour space).
    while q:
        y, x = q.popleft()
        for dy, dx in ((-1, 0), (1, 0), (0, -1), (0, 1)):
            ny, nx = y + dy, x + dx
            if 0 <= ny < h and 0 <= nx < w and not visited[ny, nx]:
                diff = np.abs(rgb[ny, nx] - rgb[y, x]).max()
                if diff <= tol:
                    visited[ny, nx] = True
                    q.append((ny, nx))

    arr[visited, 3] = 0
    Image.fromarray(arr).save(out)
    return 'pil-flood-key'


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('input')
    ap.add_argument('output')
    ap.add_argument('--threshold', type=int, default=235,
                    help='white-key cutoff for the PIL fallback')
    ap.add_argument('--tol', type=int, default=24,
                    help='per-step colour tolerance for flood-key (BFS)')
    ap.add_argument('--force', choices=['rembg', 'white-key', 'flood-key'],
                    default=None,
                    help='skip auto-detection and force a backend')
    args = ap.parse_args()

    if args.force == 'white-key':
        used = via_white_key(args.input, args.output, args.threshold)
    elif args.force == 'flood-key':
        used = via_flood_key(args.input, args.output, args.tol)
    elif args.force == 'rembg':
        used = via_rembg(args.input, args.output)
    else:
        try:
            used = via_rembg(args.input, args.output)
        except ImportError:
            # Flood-key is the better fallback — preserves eye whites
            # / teeth / inside enclosed background-coloured regions.
            used = via_flood_key(args.input, args.output, args.tol)

    print(used)


if __name__ == '__main__':
    main()
