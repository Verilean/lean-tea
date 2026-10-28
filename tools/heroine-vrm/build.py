"""Put the heroine together from two sample avatars and recolour her.

    blender -b --factory-startup --python tools/heroine-vrm/build.py -- \
        BODY.vrm HAIR.vrm OUT.blend

BODY.vrm gives the body, face and dress; HAIR.vrm only its hair, which is
moved onto BODY's head with its own spring bones. The textures are then
recoloured toward the character sheet: silver hair with lavender
shadows, violet eyes, a white dress with violet where it was pink or
teal, a gold hair clip and dark violet lashes. OUT.blend still has the
avatars' own skeleton; `rig.py` moves it onto the game's.
"""

import os
import shutil
import sys

import bpy
import numpy as np
from mathutils import Matrix

argv = sys.argv[sys.argv.index("--") + 1:]
BODY, HAIR, OUT = argv[0], argv[1], argv[2]

DARK = np.array([0.44, 0.42, 0.58])      # hair in shadow
LIGHT = np.array([0.97, 0.96, 1.0])      # hair in light
VIOLET_HUE = 0.75
IRIS_HUE = 0.76
TINTS = {                                # colour factors of the face parts
    "FaceBrow": (0.62, 0.58, 0.72),
    "FaceEyelash": (0.24, 0.18, 0.34),
    "FaceEyeline": (0.28, 0.21, 0.38),
}


def load(path):
    """Import a VRM (glTF with extensions) and return the new objects."""
    tmp = os.path.splitext(OUT)[0] + "-import.glb"
    shutil.copy(path, tmp)
    before = set(bpy.data.objects)
    bpy.ops.import_scene.gltf(filepath=tmp)
    os.remove(tmp)
    new = [o for o in bpy.data.objects if o not in before]
    for o in [o for o in new if o.name.startswith("Icosphere")]:   # an exporter marker
        new.remove(o)
        bpy.data.objects.remove(o)
    return new


def swap_hair(body, hair):
    barm = next(o for o in body if o.type == "ARMATURE")
    harm = next(o for o in hair if o.type == "ARMATURE")
    old = next(o for o in body if o.type == "MESH" and o.name.startswith("Hair"))
    new = next(o for o in hair if o.type == "MESH" and o.name.startswith("Hair"))
    head = "J_Bip_C_Head"
    off = (barm.matrix_world @ barm.data.bones[head].head_local) - \
          (harm.matrix_world @ harm.data.bones[head].head_local)
    bones = {b.name: (b.head_local.copy(), b.tail_local.copy(), b.parent.name if b.parent else None)
             for b in harm.data.bones if b.name.startswith("HairJoint")}
    for o in bpy.data.objects:
        o.select_set(o is barm)
    bpy.context.view_layer.objects.active = barm
    bpy.ops.object.mode_set(mode="EDIT")
    eb = barm.data.edit_bones
    for n in [b.name for b in eb if b.name.startswith("HairJoint")]:
        eb.remove(eb[n])
    for n, (h, t, _) in bones.items():
        b = eb.new(n)
        b.head, b.tail = h + off, t + off
    for n, (_, _, p) in bones.items():
        eb[n].parent = eb[p] if p in eb else eb[head]
    bpy.ops.object.mode_set(mode="OBJECT")
    bpy.data.objects.remove(old)
    new.parent = barm
    for m in new.modifiers:
        if m.type == "ARMATURE":
            m.object = barm
    new.data.transform(Matrix.Translation(off))
    for o in [o for o in hair if o is not new]:
        bpy.data.objects.remove(o)
    for o in [o for o in bpy.data.objects if o.type == "EMPTY"]:
        bpy.data.objects.remove(o)


# ───────────────────────── recolouring ─────────────────────────

def pixels(img):
    w, h = img.size
    a = np.empty(w * h * 4, np.float32)
    img.pixels.foreach_get(a)
    return a.reshape(h, w, 4)


def store(img, a):
    img.pixels.foreach_set(a.astype(np.float32).ravel())
    img.update()
    img.pack()


def to_hsv(c):
    r, g, b = c[..., 0], c[..., 1], c[..., 2]
    mx, mn = c.max(-1), c.min(-1)
    d = mx - mn
    safe = np.where(d > 1e-6, d, 1)
    rc, gc, bc = (mx - r) / safe, (mx - g) / safe, (mx - b) / safe
    h = np.where(r == mx, bc - gc, np.where(g == mx, 2 + rc - bc, 4 + gc - rc))
    h = np.where(d > 1e-6, h / 6 % 1, 0)
    s = np.where(mx > 1e-6, d / np.where(mx > 1e-6, mx, 1), 0)
    return h, s, mx


def from_hsv(h, s, v):
    i = np.floor(h * 6).astype(int) % 6
    f = h * 6 - np.floor(h * 6)
    p, q, t = v * (1 - s), v * (1 - s * f), v * (1 - s * (1 - f))
    return np.stack([np.choose(i, [v, q, p, p, t, v]),
                     np.choose(i, [t, v, v, q, p, p]),
                     np.choose(i, [p, p, t, v, v, q])], -1)


def luma(a):
    return a[..., :3] @ np.array([0.3, 0.59, 0.11])


def paint_hair(img):
    """Lightness, stretched over the texture's range, mapped DARK → LIGHT."""
    a = pixels(img)
    L = luma(a)
    seen = a[..., 3] > 0.05
    lo, hi = np.percentile(L[seen], 2), np.percentile(L[seen], 99.5)
    t = (np.clip((L - lo) / (hi - lo + 1e-6), 0, 1) ** 0.55)[..., None]
    a[..., :3] = DARK * (1 - t) + LIGHT * t
    store(img, a)


def paint_scalp(img):
    a = pixels(img)
    a[..., :3] = LIGHT * 0.93
    a[..., 3] = 1.0
    store(img, a)


def paint_gold(img):
    a = pixels(img)
    a[..., :3] = np.array([0.55, 0.40, 0.16]) + np.clip(luma(a), 0, 1)[..., None] * np.array([0.42, 0.40, 0.30])
    store(img, a)


def paint_iris(img):
    a = pixels(img)
    h, s, v = to_hsv(a[..., :3])
    a[..., :3] = from_hsv(np.full_like(h, IRIS_HUE), np.clip(s * 1.1, 0, 0.85), np.clip(v * 1.05, 0, 1))
    store(img, a)


def paint_cloth(img):
    """Pink and teal become violet; white, gold and the rest stay."""
    a = pixels(img)
    h, s, v = to_hsv(a[..., :3])
    pick = ((((h > 0.80) | (h < 0.06)) & (s > 0.04)) | ((h > 0.35) & (h < 0.60) & (s > 0.08)))
    a[..., :3] = from_hsv(np.where(pick, VIOLET_HUE, h),
                          np.where(pick, np.clip(s * 2.2, 0, 0.75), s),
                          np.where(pick, v * 0.85, v))
    store(img, a)


def set_factor(mat, rgb):
    """The colour the imported material multiplies its texture by."""
    for n in mat.node_tree.nodes:
        if n.type == "MIX":
            for i in n.inputs:
                if not i.links and i.type == "RGBA":
                    i.default_value = (*rgb, 1)


def recolour():
    for m in bpy.data.materials:
        for key, rgb in TINTS.items():
            if key in m.name and m.users:
                set_factor(m, rgb)
    done = set()
    for o in bpy.data.objects:
        if o.type != "MESH":
            continue
        for mat in o.data.materials:
            for img in [n.image for n in mat.node_tree.nodes if n.type == "TEX_IMAGE" and n.image]:
                if img.name in done:
                    continue
                k = mat.name
                if "HAIR_02" in k:          # the hair clip
                    paint_gold(img)
                elif "HairBack" in k:       # the scalp under the hair
                    paint_scalp(img)
                elif "HAIR" in k:
                    paint_hair(img)
                elif "EyeIris" in k:
                    paint_iris(img)
                elif "CLOTH" in k:
                    paint_cloth(img)
                else:
                    continue
                if "HAIR" in k:             # hair colour comes from the texture alone
                    set_factor(mat, (1, 1, 1))
                done.add(img.name)
                print(f"build: recoloured {k}")


def main():
    bpy.ops.wm.read_factory_settings(use_empty=True)
    swap_hair(load(BODY), load(HAIR))
    recolour()
    bpy.ops.wm.save_as_mainfile(filepath=OUT)
    print(f"build: wrote {OUT}")


main()
