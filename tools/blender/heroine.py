"""Build Breezehollow's heroine in Blender and export her as a .glb.

    blender -b --factory-startup --python tools/blender/heroine.py -- \
        examples/Breezehollow/heroine.glb [--preview out.png]

Everything is made from code, so the model can be rebuilt and tweaked
by editing the numbers here:

  * body: a skin-modifier skeleton (radii per joint), subdivided, then
    painted by region — coat, sleeves, gloves, tights, boots, skin
  * head: an egg with a tapered jaw and a small nose; the face (eyes,
    lashes, brows, mouth, blush) is a texture painted with numpy and
    mapped onto the head straight from the front
  * hair: tapered ribbons (bangs, two face-framing locks, two layers of
    waist-long back hair), each a tube with an oval section, coloured by
    a texture mapped by height
  * coat: a flared tail from the waist to below the knees, open at the
    front, with a lining and a gold hem
  * armature: a body rig plus bone chains for the hair and coat tails
    (`hair_*`, `lock_*`, `coat_*`), which the game swings with springs;
    only the body bones are animated here
  * actions: idle, run, glide, hover, fly, fall — exported as glTF
    animations of those names

Blender: +Z is up and she faces -Y; glTF turns that into +Y up, +Z
forward, which is how the game faces the hero. Her left is +X.
"""

import bpy
import bmesh
import math
import os
import random
import sys
import tempfile
from mathutils import Vector, Quaternion, Matrix

argv = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
OUT = argv[0] if argv and not argv[0].startswith("--") else "heroine.glb"
PREVIEW = argv[argv.index("--preview") + 1] if "--preview" in argv else None

random.seed(7)

# ───────────────────────── palette (sRGB hex) ─────────────────────────

PALETTE = {
    "Skin":     0xf7dcd0,
    "Neck":     0xe2b6ac,   # skin in the jaw's shadow
    "Hair":     0xdde2f0,   # silver with a cold tint
    "HairDark": 0x9aa3bd,   # brows
    "Coat":     0x1f2542,   # deep navy
    "Lining":   0x4a3f7a,
    "Trim":     0xd8b46a,   # gold
    "Inner":    0xf2f0f5,   # blouse
    "Legs":     0x26232f,
    "Dark":     0x16161d,   # gloves, boots
    "Belt":     0x3a2f2c,
    "EyeWhite": 0xffffff,
    "Iris":     0x58c4ff,   # ice blue
    "Pupil":    0x1b3a6b,
    "EyeLine":  0x1a1622,
    "Shine":    0xffffff,
    "Mouth":    0xb8676a,
    "Gem":      0x7fe8ff,
}


def lin(c):
    c = c / 255.0
    return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4


MATS = {}


def material(name):
    if name in MATS:
        return MATS[name]
    if name == "Face":
        return textured_material("Face", packed_image("face", paint_face()))
    if name == "Hair":
        return textured_material("Hair", packed_image("hair", paint_hair()))
    h = PALETTE[name]
    rgb = (lin((h >> 16) & 255), lin((h >> 8) & 255), lin(h & 255))
    m = bpy.data.materials.new(name)
    m.diffuse_color = (*rgb, 1)
    bsdf = m.node_tree.nodes.get("Principled BSDF")
    bsdf.inputs["Base Color"].default_value = (*rgb, 1)
    bsdf.inputs["Roughness"].default_value = 0.6
    if name in ("Gem", "Iris"):
        bsdf.inputs["Emission Color"].default_value = (*rgb, 1)
        bsdf.inputs["Emission Strength"].default_value = 1.5 if name == "Gem" else 0.4
    MATS[name] = m
    return m


# ───────────────────────── scene helpers ─────────────────────────

def clear_scene():
    for o in list(bpy.data.objects):
        bpy.data.objects.remove(o, do_unlink=True)


def link(obj):
    bpy.context.scene.collection.objects.link(obj)
    return obj


def activate(obj):
    for o in bpy.context.scene.objects:
        if o is not None:
            o.select_set(False)
    bpy.context.view_layer.objects.active = obj
    obj.select_set(True)


def apply_modifiers(obj):
    activate(obj)
    for m in list(obj.modifiers):
        bpy.ops.object.modifier_apply(modifier=m.name)


def smooth(obj):
    for p in obj.data.polygons:
        p.use_smooth = True


def mesh_from_bmesh(name, bm, mats=()):
    me = bpy.data.meshes.new(name)
    for n in mats:
        me.materials.append(material(n))
    bm.to_mesh(me)
    bm.free()
    return link(bpy.data.objects.new(name, me))


def set_materials(obj, names):
    obj.data.materials.clear()
    for n in names:
        obj.data.materials.append(material(n))


def catmull(points, per_seg=4):
    """Samples of a Catmull-Rom curve through `points` (ends clamped)."""
    pts = [points[0]] + list(points) + [points[-1]]
    out = []
    for i in range(1, len(pts) - 2):
        p0, p1, p2, p3 = pts[i - 1], pts[i], pts[i + 1], pts[i + 2]
        for k in range(per_seg):
            t = k / per_seg
            t2, t3 = t * t, t * t * t
            out.append(0.5 * ((2 * p1) + (-p0 + p2) * t + (2 * p0 - 5 * p1 + 4 * p2 - p3) * t2
                              + (-p0 + 3 * p1 - 3 * p2 + p3) * t3))
    out.append(points[-1])
    return out


# ───────────────────────── proportions ─────────────────────────
# Metres, about 1.82 m to the crown — an adult, not a chibi.

# The face is designed on a head of BASE_R at BASE_C (FACE_BOX and the
# sizes in paint_face are in those metres); the real head is HEAD_K times
# larger, a little under seven heads tall, as anime proportions go.
BASE_C = Vector((0, -0.005, 1.705))
BASE_R = Vector((0.092, 0.102, 0.118))
HEAD_K = 1.15
HEAD_C = Vector((0, -0.005, 1.717))
HEAD_R = BASE_R * HEAD_K

JOINTS = {
    "pelvis":  ((0, 0, 0.96), (0.125, 0.088)),
    "waist":   ((0, 0, 1.10), (0.096, 0.071)),
    "chest":   ((0, 0, 1.29), (0.124, 0.090)),
    "upchest": ((0, 0, 1.41), (0.118, 0.082)),
    "neckb":   ((0, 0, 1.50), (0.046, 0.044)),
    "neckt":   ((0, -0.004, 1.62), (0.037, 0.036)),
}
for s, side in ((1, "L"), (-1, "R")):
    JOINTS.update({
        f"sh_{side}":   ((s * 0.150, 0, 1.455), (0.050, 0.048)),
        f"elb_{side}":  ((s * 0.285, 0.0, 1.225), (0.035, 0.035)),
        f"wri_{side}":  ((s * 0.385, -0.015, 1.005), (0.026, 0.021)),
        f"hnd_{side}":  ((s * 0.425, -0.025, 0.915), (0.029, 0.013)),
        f"hip_{side}":  ((s * 0.088, 0, 0.90), (0.079, 0.079)),
        f"knee_{side}": ((s * 0.098, 0.0, 0.505), (0.049, 0.051)),
        f"ank_{side}":  ((s * 0.100, 0.012, 0.085), (0.034, 0.036)),
        f"toe_{side}":  ((s * 0.100, -0.105, 0.025), (0.032, 0.021)),
    })
BODY_EDGES = [("pelvis", "waist"), ("waist", "chest"), ("chest", "upchest"),
              ("upchest", "neckb"), ("neckb", "neckt")]
for side in "LR":
    BODY_EDGES += [("upchest", f"sh_{side}"), (f"sh_{side}", f"elb_{side}"),
                   (f"elb_{side}", f"wri_{side}"), (f"wri_{side}", f"hnd_{side}"),
                   ("pelvis", f"hip_{side}"), (f"hip_{side}", f"knee_{side}"),
                   (f"knee_{side}", f"ank_{side}"), (f"ank_{side}", f"toe_{side}")]


def J(name):
    return Vector(JOINTS[name][0])


# ───────────────────────── body ─────────────────────────

def build_body():
    names = list(JOINTS)
    me = bpy.data.meshes.new("Body")
    me.from_pydata([JOINTS[n][0] for n in names],
                   [(names.index(a), names.index(b)) for a, b in BODY_EDGES], [])
    obj = link(bpy.data.objects.new("Body", me))
    skin = obj.modifiers.new("Skin", "SKIN")
    skin.branch_smoothing = 0.6
    for i, n in enumerate(names):
        v = me.skin_vertices[0].data[i]
        v.radius = JOINTS[n][1]
        v.use_root = n == "pelvis"
    sub = obj.modifiers.new("Sub", "SUBSURF")
    sub.levels = 2
    apply_modifiers(obj)
    smooth(obj)
    order = ["Coat", "Inner", "Trim", "Dark", "Legs", "Neck", "Belt"]
    set_materials(obj, order)
    for p in obj.data.polygons:
        c = p.center
        ax = abs(c.x)
        if c.z > 1.49 and ax < 0.08:
            mat = "Neck"
        elif ax > 0.135 and c.z > 0.86:                    # arm
            side = "L" if c.x > 0 else "R"
            e, w = J(f"elb_{side}"), J(f"wri_{side}")
            along = (c - e).dot((w - e).normalized()) - (w - e).length
            mat = "Dark" if along > -0.005 else "Trim" if along > -0.03 else "Coat"
        elif c.z >= 0.92:
            mat = "Coat"
            if 1.065 < c.z < 1.115:
                mat = "Belt"
        else:
            mat = "Dark" if c.z < 0.60 else "Legs"         # boots, tights
        p.material_index = order.index(mat)
    return obj


def build_head():
    """A soft egg with a small chin. Its front half is UV-mapped straight
    from the front onto the face texture, and its normals are those of a
    plain ellipsoid, so the toon shadow falls across the face in one clean
    shape instead of breaking up around the nose and cheeks."""
    bm = bmesh.new()
    bmesh.ops.create_uvsphere(bm, u_segments=48, v_segments=32, radius=1.0)
    normals = {}
    for v in bm.verts:
        d = v.co.copy()
        normals[v.index] = Vector((d.x / HEAD_R.x, d.y / HEAD_R.y, d.z / HEAD_R.z)).normalized()
        v.co = head_point(*d)
    uv = bm.loops.layers.uv.new("UVMap")
    x0, x1, z0, z1 = FACE_BOX
    for f in bm.faces:
        front = f.calc_center_median().y < HEAD_C.y + 0.01
        for l in f.loops:
            q = BASE_C + (l.vert.co - HEAD_C) / HEAD_K       # where it lies on the design head
            l[uv].uv = ((q.x - x0) / (x1 - x0), (q.z - z0) / (z1 - z0)) if front else (0.01, 0.01)
    obj = mesh_from_bmesh("Head", bm)
    smooth(obj)
    obj.data.normals_split_custom_set_from_vertices([normals[i] for i in range(len(obj.data.vertices))])
    set_materials(obj, ["Face"])
    return obj


def head_point(x, y, z):
    """A point of the unit sphere moved onto the head's surface: round
    cheeks narrowing to a small chin, a short lower face, a flat front."""
    def ramp(v, a, b):
        return max(0.0, min(1.0, (v - a) / (b - a)))
    if z < 0:
        t = -z
        r = max(math.sqrt(max(0.0, 1 - t * t)), 1e-3)
        dx, dy = x / r, y / r                           # direction around the head
        wid = 1 - 0.06 * t if t < 0.30 else 0.982 - 0.76 * ((t - 0.30) / 0.70) ** 1.8
        if t > 0.86:                                    # round the chin off
            wid = max(wid, 0.24 * math.sqrt(max(0.0, 1 - ((t - 1) / 0.14) ** 2)))
        x = dx * wid
        y = dy * ((1 - 0.20 * t) if dy < 0 else (1 - 0.25 * t))
        rise = ramp(dy, -0.5, 1.0)
        rise = rise * rise * (3 - 2 * rise)
        z = -0.88 * t * (1 - 0.38 * rise)              # short face; jaw rising softly to the ear
    if y < 0:
        y *= 1 - 0.05 * math.exp(-(x / 0.55) ** 2)    # a flatter face plane
    # just the tip of a nose
    y -= 0.05 * math.exp(-(x / 0.07) ** 2 - ((z + 0.33) / 0.07) ** 2) * max(0.0, -y)
    return Vector((x * HEAD_R.x, y * HEAD_R.y, z * HEAD_R.z)) + HEAD_C


def ellipsoid(bm, center, radii, rot=(0, 0, 0), segs=(12, 8)):
    """A flattened sphere added to `bm`; returns its verts."""
    geom = bmesh.ops.create_uvsphere(bm, u_segments=segs[0], v_segments=segs[1], radius=1.0)
    verts = geom["verts"]
    rm = (Matrix.Rotation(rot[2], 3, "Z") @ Matrix.Rotation(rot[1], 3, "Y")
          @ Matrix.Rotation(rot[0], 3, "X"))
    for v in verts:
        v.co = rm @ Vector((v.co.x * radii[0], v.co.y * radii[1], v.co.z * radii[2])) + Vector(center)
    return verts


# ───────────────────────── face texture ─────────────────────────
# The face is painted, not modelled: eyes, lashes, brows, nose, mouth and
# blush are drawn with numpy into an image that covers FACE_BOX (metres,
# seen from the front), so every size below is in metres on the face.

FACE_TEX = (1024, 1280)                      # pixels, 0.2 mm each
FACE_BOX = (-0.100, 0.100, 1.580, 1.830)     # x0, x1, z0, z1
EYE_X, EYE_Z = 0.038, 1.688                  # centre of her left eye
EYE_HW = 0.0215                              # half width of the opening
EYE_TILT = 0.10                              # outer corners up


def hexrgb(h):
    return ((h >> 16) & 255) / 255.0, ((h >> 8) & 255) / 255.0, (h & 255) / 255.0


def paint_face():
    """The face texture as an sRGB float array (rows from the chin up)."""
    import numpy as np
    W, H = FACE_TEX
    x0, x1, z0, z1 = FACE_BOX
    px = (x1 - x0) / W
    X, Z = np.meshgrid(x0 + (np.arange(W) + 0.5) * px, z0 + (np.arange(H) + 0.5) * px)
    img = np.empty((H, W, 3))
    img[:] = hexrgb(PALETTE["Skin"])

    def cover(d):                                 # signed distance → coverage
        return np.clip(0.5 - d / px, 0.0, 1.0)

    def over(a, col, sl=(slice(None), slice(None))):
        a = a[..., None]
        img[sl] = img[sl] * (1 - a) + np.array(hexrgb(col) if isinstance(col, int) else col) * a

    def stroke(pts, radii, col, alpha=1.0, clip=None):
        """A tapered line through `pts` [(x, z)], radius per point."""
        P, R = np.array(pts), np.array(radii, dtype=float)
        pad = R.max() + 3 * px
        i0 = max(0, int((P[:, 1].min() - pad - z0) / px)); i1 = min(H, int((P[:, 1].max() + pad - z0) / px) + 1)
        j0 = max(0, int((P[:, 0].min() - pad - x0) / px)); j1 = min(W, int((P[:, 0].max() + pad - x0) / px) + 1)
        sl = (slice(i0, i1), slice(j0, j1))
        xs, zs = X[sl], Z[sl]
        d = np.full(xs.shape, np.inf)
        for (a, b), (ra, rb) in zip(zip(P, P[1:]), zip(R, R[1:])):
            ab = b - a
            t = np.clip(((xs - a[0]) * ab[0] + (zs - a[1]) * ab[1]) / max(ab.dot(ab), 1e-12), 0, 1)
            d = np.minimum(d, np.hypot(xs - a[0] - t * ab[0], zs - a[1] - t * ab[1]) - (ra + t * (rb - ra)))
        a = cover(d) * alpha
        if clip is not None:
            a = a * clip[sl]
        over(a, col, sl)

    def soft(cx, cz, rx, rz, col, alpha):
        over(alpha * np.exp(-((X - cx) / rx) ** 2 - ((Z - cz) / rz) ** 2), col)

    def ellipse(cx, cz, rx, rz):                  # approximate signed distance
        return (np.hypot((X - cx) / rx, (Z - cz) / rz) - 1) * min(rx, rz)

    # blush, and a faint shade under the brow ridge
    for s in (1, -1):
        soft(s * 0.050, 1.664, 0.015, 0.0060, 0xf6a2a6, 0.30)
        soft(s * 0.037, 1.712, 0.022, 0.006, 0xe9c0b6, 0.35)

    for s in (1, -1):
        ex = s * EYE_X
        def to_xz(u, w, ex=ex, s=s):              # eye frame: u outward, w up (tilted)
            return ex + s * u, EYE_Z + w + EYE_TILT * u
        def upper(u):
            q = np.clip(np.abs(u) / EYE_HW, 0, 1)
            return 0.0152 * (1 - q * q) ** 0.48 * (1 - 0.18 * np.clip(u / EYE_HW, -1, 1)) - 0.0004
        def lower(u):
            q = np.clip(np.abs(u) / EYE_HW, 0, 1)
            return -0.0118 * (1 - q * q) ** 0.62 + 0.0006
        U = s * (X - ex)
        Wt = Z - EYE_Z - EYE_TILT * U
        d_open = np.maximum.reduce([Wt - upper(U), lower(U) - Wt, np.abs(U) - EYE_HW])
        inside = cover(d_open)

        # crease of the eyelid
        us = np.linspace(-0.55, 1.05, 12) * EYE_HW
        stroke([to_xz(u, upper(u) + 0.0048 + 0.0008 * (u / EYE_HW)) for u in us],
               [0.00035 + 0.00025 * math.sin(math.pi * k / 11) for k in range(12)], 0xc58c86, 0.85)
        # the white, shaded under the lid
        over(inside, 0xfbfbff)
        lid = np.clip((Wt - upper(U) + 0.0042) / 0.0042, 0, 1)
        over(inside * lid * 0.75, 0xb9c4e2)

        # iris: dark at the top, light at the bottom, with a rim
        icx, icz, irx, irz = ex - s * 0.0008, EYE_Z - 0.0012, 0.0128, 0.0170
        iris = cover(ellipse(icx, icz, irx, irz)) * inside
        t = np.clip((Z - icz) / irz, -1, 1)
        top, mid, bot = np.array(hexrgb(0x1b2f7a)), np.array(hexrgb(0x2f86e8)), np.array(hexrgb(0xa8f0ff))
        grad = np.where(t[..., None] > 0, mid + (top - mid) * t[..., None] ** 0.8,
                        mid + (bot - mid) * (-t[..., None]) ** 1.2)
        over(iris, grad)
        # fine streaks toward the pupil
        ang = np.arctan2((Z - icz) / irz, (X - icx) / irx)
        rr = np.hypot((X - icx) / irx, (Z - icz) / irz)
        streak = (0.5 + 0.5 * np.cos(ang * 22)) * np.clip((rr - 0.45) / 0.3, 0, 1) * np.clip((0.95 - rr) / 0.2, 0, 1)
        over(iris * streak * 0.18, 0x0d2a5c)
        # a light crescent at the bottom of the iris
        over(iris * np.clip(1 - np.abs(rr - 0.72) / 0.12, 0, 1) * np.clip(-t - 0.2, 0, 1) * 0.55, 0xd2f6ff)
        over(iris * cover(np.abs(ellipse(icx, icz, irx, irz) + 0.0004) - 0.0004), 0x0f2550)
        over(cover(ellipse(icx, icz + 0.0006, 0.0042, 0.0062)) * inside * 0.85, 0x0e1a46)
        over(iris * lid * 0.55, 0x0c1f48)
        # highlights
        over(cover(ellipse(icx + s * 0.0036, icz + 0.0050, 0.0030, 0.0036)) * inside, 0xffffff)
        over(cover(ellipse(icx - s * 0.0040, icz - 0.0068, 0.0016, 0.0012)) * inside * 0.95, 0xffffff)
        over(cover(ellipse(icx - s * 0.0052, icz + 0.0028, 0.0008, 0.0008)) * inside * 0.9, 0xffffff)
        over(cover(ellipse(icx + s * 0.0058, icz - 0.0030, 0.0006, 0.0006)) * inside * 0.7, 0xe8fbff)

        # upper lash line: thick, heavier toward the outer corner, with a wing
        us = np.linspace(-1.02, 1.0, 26) * EYE_HW
        line = [to_xz(u, upper(u) + 0.0006) for u in us]
        rad = [0.0009 + 0.0017 * max(0.0, (u / EYE_HW + 1) / 2) ** 1.4 for u in us]
        wing = [to_xz(EYE_HW + k * 0.0016, 0.0006 + k * 0.0011 - 0.00005 * k * k) for k in range(1, 6)]
        stroke(line + wing, rad + [rad[-1] * (1 - k / 5.5) for k in range(1, 6)], PALETTE["EyeLine"])
        # a few lashes off the outer half
        for u0, du, dw, r in ((0.55, 0.0030, 0.0034, 0.00045), (0.78, 0.0040, 0.0030, 0.0005),
                              (0.96, 0.0052, 0.0022, 0.00055)):
            u = u0 * EYE_HW
            a = to_xz(u, upper(u) + 0.0012)
            b = to_xz(u + du, upper(u) + 0.0012 + dw)
            stroke([a, b], [r, 0.00005], PALETTE["EyeLine"])
        # lower lashes: a thin line on the outer two thirds, and a hint inside
        us = np.linspace(-0.25, 1.0, 12) * EYE_HW
        stroke([to_xz(u, lower(u) - 0.0003) for u in us],
               [0.00012 + 0.0003 * ((u / EYE_HW + 0.25) / 1.25) for u in us], 0x5a4650, 0.9)
        stroke([to_xz(-0.98 * EYE_HW, 0.0003), to_xz(-0.75 * EYE_HW, -0.0030)], [0.0003, 0.0001], 0x7a5a5e, 0.7)

        # brow: long, thin and straight, lowered at the inner end
        bp = [(-0.019, 0.0355), (-0.005, 0.0400), (0.010, 0.0418), (0.025, 0.0398)]
        stroke([(ex + s * u, EYE_Z + w) for u, w in bp], [0.0011, 0.0010, 0.0008, 0.0003], 0x8e95b2)

    # nose: a short shadow under the tip
    stroke([(0.0030, 1.6575), (0.0042, 1.6555)], [0.0003, 0.0002], 0xd6a094, 0.8)
    # mouth: a thin line, darker at the centre, and a hint of the lower lip
    stroke([(-0.0062, 1.6338), (-0.0025, 1.6328), (0.0, 1.6330), (0.0025, 1.6328), (0.0062, 1.6340)],
           [0.00012, 0.0004, 0.00045, 0.0004, 0.00012], 0xa4545c)
    return img


def packed_image(name, img):
    """An sRGB float array (rows from the bottom up) as a packed PNG image."""
    import numpy as np
    H, W = img.shape[:2]
    rgba = np.concatenate([img, np.ones((H, W, 1))], axis=2).astype(np.float32)
    tmp = bpy.data.images.new(name + "-tmp", W, H, alpha=False)
    tmp.pixels.foreach_set(rgba.ravel())
    # round-trip through a PNG so the image is an ordinary packed file
    # (a generated image renders blank in Cycles)
    path = os.path.join(tempfile.mkdtemp(), name + ".png")
    tmp.filepath_raw = path
    tmp.file_format = "PNG"
    tmp.save()
    bpy.data.images.remove(tmp)
    im = bpy.data.images.load(path)
    im.name = name
    im.pack()
    os.remove(path)
    return im


HAIR_TEX_Z = (0.90, 1.95)          # heights the hair texture spans
HAIR_RING_Z = 1.80                 # the band of shine round the crown


def paint_hair():
    """Two columns, inner and outer, from the tips (row 0) up to the crown:
    silver fading to ice blue at the tips, with a band of shine on top."""
    import numpy as np
    W, H = 64, 512
    z = HAIR_TEX_Z[0] + (np.arange(H) + 0.5) / H * (HAIR_TEX_Z[1] - HAIR_TEX_Z[0])
    keys = [(0.90, 0xa9c8ef), (1.15, 0xc3d6f2), (1.50, 0xdfe5f4), (1.75, 0xe9edf8), (1.95, 0xf1f3fb)]
    zs = [k[0] for k in keys]
    col = np.stack([np.interp(z, zs, [hexrgb(k[1])[c] for k in keys]) for c in range(3)], axis=1)
    ring = np.exp(-((z - HAIR_RING_Z) / 0.010) ** 2)
    under = np.exp(-((z - (HAIR_RING_Z - 0.024)) / 0.012) ** 2)
    outer = col * (1 - 0.06 * under)[:, None]
    outer = outer + (1 - outer) * (0.85 * ring)[:, None]
    inner = col * np.array([0.80, 0.81, 0.88])
    img = np.empty((H, W, 3))
    img[:, : W // 2] = inner[:, None, :]
    img[:, W // 2:] = outer[:, None, :]
    return img


def textured_material(name, image):
    m = bpy.data.materials.new(name)
    m.diffuse_color = (*(lin(int(255 * c)) for c in hexrgb(PALETTE.get(name, 0xffffff))), 1)
    nt = m.node_tree
    bsdf = nt.nodes.get("Principled BSDF")
    bsdf.inputs["Roughness"].default_value = 0.6
    tex = nt.nodes.new("ShaderNodeTexImage")
    tex.image = image
    tex.interpolation = "Linear"
    nt.links.new(tex.outputs["Color"], bsdf.inputs["Base Color"])
    MATS[name] = m
    return m


def build_face():
    """The ears, and the hair gem with its gold clasp (the face is painted)."""
    gem_c = HEAD_C + Vector((0.102, 0.01, 0.055)) * HEAD_K
    bm = bmesh.new()
    for f in {f for v in ellipsoid(bm, gem_c, Vector((0.014, 0.008, 0.022)) * HEAD_K, (0.3, 0.4, 0.5), (6, 4)) for f in v.link_faces}:
        f.material_index = 0
    for f in {f for v in ellipsoid(bm, gem_c + Vector((0, 0.004, -0.028)) * HEAD_K, Vector((0.008, 0.006, 0.008)) * HEAD_K) for f in v.link_faces}:
        f.material_index = 1
    for s in (1, -1):                                   # ears, mostly under the hair
        for f in {f for v in ellipsoid(bm, HEAD_C + Vector((s * 0.088, 0.017, -0.023)) * HEAD_K, Vector((0.009, 0.015, 0.026)) * HEAD_K,
                                         (0.0, -s * 0.25, 0.0), (12, 8)) for f in v.link_faces}:
            f.material_index = 2
    obj = mesh_from_bmesh("Face", bm, ["Gem", "Trim", "Skin"])
    smooth(obj)
    return obj


def build_buttons(body):
    """Gold buttons in two rows down the coat front and a white cravat at the
    throat, placed on the body's surface by casting rays at it."""
    bm = bmesh.new()
    order = ["Trim", "Inner", "Gem"]
    def on_body(x, z):
        hit, loc, nrm, _ = body.ray_cast(Vector((x, -0.6, z)), Vector((0, 1, 0)))
        return loc if hit else Vector((x, -0.09, z))
    for z in (1.37, 1.285, 1.20):
        for x in (-0.036, 0.036):
            p = on_body(x, z)
            for v in ellipsoid(bm, p + Vector((0, -0.002, 0)), (0.0085, 0.004, 0.0085), segs=(10, 6)):
                for f in v.link_faces:
                    f.material_index = 0
    # a ruffled jabot, widening in tiers, pinned by a gem brooch
    parts = [(1, z, r, -0.004) for z, r in ((1.470, (0.016, 0.005, 0.012)),
                                           (1.450, (0.020, 0.005, 0.013)),
                                           (1.430, (0.023, 0.005, 0.012)))]
    parts += [(0, 1.488, (0.0125, 0.004, 0.0125), -0.010), (2, 1.488, (0.0085, 0.004, 0.0085), -0.013)]
    for mi, z, r, dy in parts:
        p = on_body(0, z)
        for v in ellipsoid(bm, p + Vector((0, dy, 0)), r, segs=(14, 8)):
            for f in v.link_faces:
                f.material_index = mi
    obj = mesh_from_bmesh("Buttons", bm, order)
    smooth(obj)
    return obj


# ───────────────────────── hair ─────────────────────────
# Each strand is a tube with an oval section along a smooth path,
# tapering to a point. Weights are recorded as the strand is built:
# near the scalp it follows the head; below, the chain bones.

HAIR_WEIGHTS = []    # per vertex, in creation order: {bone: weight}
HAIR_SHADE = []      # per vertex: column of the hair texture (OUTER or INNER)
OUTER, INNER = 0.75, 0.25
SHADE = [OUTER]      # the column strands being built now take


def cap_point(a, elev, scale):
    """A point on the head, `a` around from the back (+Y), `elev` up."""
    return HEAD_C + Vector((math.sin(a) * math.cos(elev) * HEAD_R.x * scale,
                            math.cos(a) * math.cos(elev) * HEAD_R.y * scale,
                            math.sin(elev) * HEAD_R.z * scale))


def strand(bm, path, width, thick, side, weight_at, taper_from=0.75, root_w=0.45, per_seg=4):
    pts = catmull(path, per_seg)
    n = len(pts)
    rings = []
    for k, p in enumerate(pts):
        s = k / (n - 1)
        t = (pts[min(k + 1, n - 1)] - pts[max(k - 1, 0)]).normalized()
        sv = (side(p) if callable(side) else Vector(side))
        sv = (sv - t * sv.dot(t)).normalized()
        nv = t.cross(sv).normalized()
        grow = root_w + (1 - root_w) * min(1.0, s / 0.12)
        shrink = 1.0 if s < taper_from else max(0.0, (1 - s) / (1 - taper_from)) ** 0.8
        w, th = width * grow * shrink, thick * grow * max(0.35, shrink)
        if k == n - 1:
            v = bm.verts.new(p)
            HAIR_WEIGHTS.append(weight_at(p, s))
            HAIR_SHADE.append(SHADE[0])
            rings.append([v])
            continue
        ring = []
        for j in range(6):
            ang = j * math.pi / 3
            v = bm.verts.new(p + sv * (w * math.cos(ang)) + nv * (th * math.sin(ang)))
            HAIR_WEIGHTS.append(weight_at(p, s))
            HAIR_SHADE.append(SHADE[0])
            ring.append(v)
        rings.append(ring)
    for a, b in zip(rings, rings[1:]):
        if len(b) == 1:
            for j in range(6):
                bm.faces.new((a[j], a[(j + 1) % 6], b[0]))
        else:
            for j in range(6):
                bm.faces.new((a[j], a[(j + 1) % 6], b[(j + 1) % 6], b[j]))
    bm.faces.new(list(reversed(rings[0])))


def chain_weights(p, chains, levels, head_from):
    """Blend between chains by x and between levels by z (+ head near the scalp)."""
    ws = {}
    xs = [c[1] for c in chains]
    if p.x <= xs[0]:
        across = [(chains[0][0], 1.0)]
    elif p.x >= xs[-1]:
        across = [(chains[-1][0], 1.0)]
    else:
        i = max(k for k in range(len(xs) - 1) if xs[k] <= p.x)
        f = (p.x - xs[i]) / (xs[i + 1] - xs[i])
        across = [(chains[i][0], 1 - f), (chains[i + 1][0], f)]
    # continuous index along the chain: bone i spans levels[i] → levels[i+1]
    z = p.z
    nb = len(levels) - 1
    if z >= levels[0]:
        t = 0.0
    elif z <= levels[-1]:
        t = nb - 0.5
    else:
        i = max(k for k in range(nb) if levels[k] >= z)
        t = i + (levels[i] - z) / (levels[i] - levels[i + 1])
    c = min(max(t - 0.5, 0.0), nb - 1)
    lo = int(math.floor(c))
    hi = min(lo + 1, nb - 1)
    fb = c - lo
    head = max(0.0, min(1.0, (z - head_from) / 0.05))
    for name, wa in across:
        for idx, wb in ((lo, 1 - fb), (hi, fb)):
            k = f"{name}_{idx}"
            ws[k] = ws.get(k, 0) + wa * wb * (1 - head)
    if head > 0:
        ws["head"] = head
    return ws


BACK_LEVELS = [1.62, 1.45, 1.28, 1.12, 0.98]
BACK_CHAINS = [("hair_R", -0.075), ("hair_C", 0.0), ("hair_L", 0.075)]
LOCK_LEVELS = [1.62, 1.51, 1.41, 1.31]


def back_path(a, layer, z_end):
    sc = 1.08 + 0.05 * layer
    p0 = cap_point(a, math.radians(52), sc - 0.02)
    p1 = cap_point(a, math.radians(12), sc + 0.06)
    p2 = cap_point(a, math.radians(-28), sc + 0.05)
    out = 0.012 * layer
    p3 = Vector((math.sin(a) * (0.112 + out), 0.100 + math.cos(a) * 0.040 + out, 1.50))
    p4 = Vector((math.sin(a) * (0.128 + out), 0.118 + math.cos(a) * 0.034 + out, 1.30))
    p5 = Vector((math.sin(a) * (0.148 + out), 0.124 + math.cos(a) * 0.030 + out, z_end))
    return [p0, p1, p2, p3, p4, p5]


def build_hair():
    bm = bmesh.new()
    head_only = lambda p, s: {"head": 1.0}
    back_w = lambda p, s: chain_weights(p, BACK_CHAINS, BACK_LEVELS, 1.60)
    # hair cap: the top and back of the head, with some volume on top
    geom = bmesh.ops.create_uvsphere(bm, u_segments=32, v_segments=20, radius=1.0)
    keep = []
    for v in geom["verts"]:
        x, y, z = v.co
        puff = 1.10 + 0.03 * max(0.0, z)
        v.co = Vector((x * HEAD_R.x * puff, y * HEAD_R.y * (puff - 0.02), z * HEAD_R.z * 1.08)) + HEAD_C + Vector((0, 0.004, 0.008))
        keep.append(v)
    k = HEAD_K
    kill = [v for v in keep if (v.co - HEAD_C).y < -0.02 * k and (v.co - HEAD_C).z < 0.055 * k
            and abs((v.co - HEAD_C).x) < 0.080 * k or (v.co - HEAD_C).z < -0.075 * k]
    bmesh.ops.delete(bm, geom=kill, context="VERTS")
    for _ in [v for v in bm.verts]:
        HAIR_WEIGHTS.append({"head": 1.0})
        HAIR_SHADE.append(OUTER)
    # back hair, two layers, longest in the middle (a soft V)
    for layer, count, span in ((0, 11, 1.40), (1, 10, 1.25)):
        SHADE[0] = INNER if layer == 0 else OUTER
        for i in range(count):
            a = -span + 2 * span * i / (count - 1) + (0.0 if layer == 0 else span / (count - 1) * 0.0)
            z_end = 0.985 + 0.10 * (abs(a) / 1.40) ** 1.6 + random.uniform(-0.012, 0.012) + 0.03 * layer
            side = Vector((math.cos(a), -math.sin(a), 0))
            strand(bm, back_path(a, layer, z_end), 0.027 - 0.003 * layer, 0.010, side, back_w,
                   taper_from=0.80, per_seg=5)
    SHADE[0] = OUTER
    # bangs: thin flat strands from the crown, swept a little to her right,
    # their tips staggered just above the eyes
    tips = [10, 6, 13, 3, 8, 0, 5, -1, 7, 2, 11, 6, 14]
    for i, el in enumerate(tips):
        u = -1 + 2 * i / (len(tips) - 1)
        a = math.pi + u * 1.10
        sweep = Vector((-0.016, 0, 0))
        p0 = cap_point(a + 0.18, math.radians(76), 1.04)
        p1 = cap_point(a + 0.04, math.radians(42), 1.12)
        p2 = cap_point(a - 0.08, math.radians(20), 1.115) + sweep * 0.5
        p3 = cap_point(a - 0.16, math.radians(el + 6 * abs(u) ** 2), 1.075 + 0.025 * abs(u)) + sweep
        side = Vector((math.cos(a), -math.sin(a), 0))
        strand(bm, [p0, p1, p2, p3], 0.0145 + 0.003 * (i % 2), 0.0045, side, head_only,
               taper_from=0.35, root_w=0.7)
    # face-framing locks, to the chest
    for s, name in ((1, "lock_L"), (-1, "lock_R")):
        lock_w = lambda p, t, n=name: chain_weights(p, [(n, 0.0)], LOCK_LEVELS, 1.60)
        for off in (0.0, 0.22):
            a = math.pi - s * (1.18 + off)
            path = [cap_point(a, math.radians(45), 1.10), cap_point(a, math.radians(5), 1.14),
                    cap_point(a, math.radians(-38), 1.12),
                    Vector((s * (0.090 + 0.012 * off * 4), -0.085 + off * 0.05, 1.49)),
                    Vector((s * (0.084 + 0.012 * off * 4), -0.102 + off * 0.05, 1.33 + off * 0.12))]
            side = lambda p, ss=s: Vector((0, -1, 0)).cross(Vector((0, 0, 1))) * ss
            strand(bm, path, 0.020 - off * 0.02, 0.009, lambda p: Vector((math.cos(a), -math.sin(a), 0)),
                   lock_w, taper_from=0.6, per_seg=4)
    obj = mesh_from_bmesh("Hair", bm)
    smooth(obj)
    # UVs for the hair texture: v by height, u picks the outer or inner column
    me = obj.data
    uv = me.uv_layers.new(name="UVMap")
    z0, z1 = HAIR_TEX_Z
    for l in me.loops:
        vi = l.vertex_index
        uv.data[l.index].uv = (HAIR_SHADE[vi], (me.vertices[vi].co.z - z0) / (z1 - z0))
    set_materials(obj, ["Hair"])
    return obj


# ───────────────────────── coat tail and collar ─────────────────────────

COAT_CHAINS = [("coat_FR", -0.80), ("coat_BR", -2.05), ("coat_B", math.pi),
               ("coat_BL", 2.05), ("coat_FL", 0.80)]


def coat_point(th, s, inset=1.0):
    """The coat tail at angle `th` (0 = front, + = her left) and depth s (0 waist .. 1 hem)."""
    e = s ** 0.85
    rx = (0.112 + (0.275 - 0.112) * e) * inset
    ry = (0.083 + (0.225 - 0.083) * e) * inset
    hem = 0.42 - 0.08 * (1 - math.cos(th)) / 2
    return Vector((math.sin(th) * rx, -math.cos(th) * ry, 1.08 + (hem - 1.08) * s))


def angle_weights(th):
    """The two nearest coat chains around the body, blended."""
    angs = [(n, a % (2 * math.pi)) for n, a in COAT_CHAINS]
    th = th % (2 * math.pi)
    angs.sort(key=lambda na: na[1])
    for (n0, a0), (n1, a1) in zip(angs, angs[1:]):
        if a0 <= th <= a1:
            f = (th - a0) / (a1 - a0)
            return [(n0, 1 - f), (n1, f)]
    return [(angs[0][0] if th < angs[0][1] else angs[-1][0], 1.0)]


def build_coat():
    bm = bmesh.new()
    cols, rows = 44, 14
    grid = []
    for r in range(rows + 1):
        s = r / rows
        gap = 0.16 + 0.50 * s
        row = []
        for c in range(cols + 1):
            th = gap + (2 * math.pi - 2 * gap) * c / cols
            row.append(bm.verts.new(coat_point(th, s)))
            ws = {}
            hips = max(0.0, 1 - s / 0.18)
            lv = min(s * 3, 2.999)
            lo, f = int(lv), lv - int(lv)
            for name, wa in angle_weights(th):
                for idx, wb in ((lo, 1 - f), (min(lo + 1, 3), f)):
                    if idx > 2:
                        idx = 2
                    k = f"{name}_{idx}"
                    ws[k] = ws.get(k, 0) + wa * wb * (1 - hips)
            if hips > 0:
                ws["hips"] = hips
            HAIR_WEIGHTS_COAT.append(ws)
        grid.append(row)
    for r in range(rows):
        for c in range(cols):
            f = bm.faces.new((grid[r][c], grid[r + 1][c], grid[r + 1][c + 1], grid[r][c + 1]))
            edge = c < 1 or c >= cols - 1 or r >= rows - 1
            f.material_index = 1 if edge else 0
    obj = mesh_from_bmesh("CoatTail", bm, ["Coat", "Trim", "Lining"])
    sol = obj.modifiers.new("Solid", "SOLIDIFY")
    sol.thickness = 0.008
    sol.offset = -1.0
    sol.material_offset = 2
    sol.use_rim = True
    obj.modifiers.new("Sub", "SUBSURF").levels = 1
    return obj


HAIR_WEIGHTS_COAT = []


def build_collar():
    bm = bmesh.new()
    cols, rows = 28, 3
    grid = []
    for r in range(rows + 1):
        s = r / rows
        row = []
        for c in range(cols + 1):
            th = 0.30 + (2 * math.pi - 0.60) * c / cols
            rx, ry = 0.066 - 0.006 * s, 0.063 - 0.004 * s
            z = 1.47 + 0.115 * s + 0.02 * (1 - math.cos(th)) / 2 * s
            row.append(bm.verts.new(Vector((math.sin(th) * rx, -math.cos(th) * ry - 0.002, z))))
        grid.append(row)
    for r in range(rows):
        for c in range(cols):
            f = bm.faces.new((grid[r][c], grid[r][c + 1], grid[r + 1][c + 1], grid[r + 1][c]))
            f.material_index = 1 if r == rows - 1 else 0
    obj = mesh_from_bmesh("Collar", bm, ["Coat", "Trim", "Lining"])
    sol = obj.modifiers.new("Solid", "SOLIDIFY")
    sol.thickness = 0.006
    sol.offset = 1.0
    sol.material_offset = 2
    obj.modifiers.new("Sub", "SUBSURF").levels = 1
    return obj


# ───────────────────────── armature ─────────────────────────

BODY_BONES = {
    # name: (head, tail, parent)
    "root":  ((0, 0, 0), (0, 0, 0.15), None),
    "hips":  ((0, 0, 0.95), (0, 0, 1.10), "root"),
    "spine": ((0, 0, 1.10), (0, 0, 1.30), "hips"),
    "chest": ((0, 0, 1.30), (0, 0, 1.50), "spine"),
    "neck":  ((0, 0, 1.50), (0, -0.004, 1.62), "chest"),
    "head":  ((0, -0.004, 1.62), (0, -0.004, 1.84), "neck"),
}
for s, side in ((1, "L"), (-1, "R")):
    BODY_BONES.update({
        f"shoulder_{side}":  ((s * 0.03, 0, 1.45), tuple(J(f"sh_{side}")), "chest"),
        f"upper_arm_{side}": (tuple(J(f"sh_{side}")), tuple(J(f"elb_{side}")), f"shoulder_{side}"),
        f"forearm_{side}":   (tuple(J(f"elb_{side}")), tuple(J(f"wri_{side}")), f"upper_arm_{side}"),
        f"hand_{side}":      (tuple(J(f"wri_{side}")), tuple(J(f"hnd_{side}")), f"forearm_{side}"),
        f"thigh_{side}":     ((s * 0.088, 0, 0.93), tuple(J(f"knee_{side}")), "hips"),
        f"shin_{side}":      (tuple(J(f"knee_{side}")), tuple(J(f"ank_{side}")), f"thigh_{side}"),
        f"foot_{side}":      (tuple(J(f"ank_{side}")), tuple(J(f"toe_{side}")), f"shin_{side}"),
    })
DEFORM_BODY = [b for b in BODY_BONES if b not in ("root",)]


def chain_bones():
    """Spring chains: name → list of points (one more than bones), parent."""
    chains = {}
    for name, x in BACK_CHAINS:
        pts = [Vector((x, 0.122 + (0.010 if i else 0), z)) for i, z in enumerate(BACK_LEVELS)]
        pts = [Vector((x * (1 + 0.5 * i / 4), 0.118 + 0.004 * i, z)) for i, z in enumerate(BACK_LEVELS)]
        chains[name] = (pts, "head")
    for s, name in ((1, "lock_L"), (-1, "lock_R")):
        pts = [Vector((s * 0.095, -0.065, 1.62)), Vector((s * 0.090, -0.088, 1.51)),
               Vector((s * 0.086, -0.100, 1.41)), Vector((s * 0.084, -0.104, 1.31))]
        chains[name] = (pts, "head")
    for name, th in COAT_CHAINS:
        pts = [coat_point(th, k / 3, 0.97) for k in range(4)]
        chains[name] = (pts, "hips")
    return chains


def build_armature():
    data = bpy.data.armatures.new("Rig")
    arm = link(bpy.data.objects.new("Rig", data))
    activate(arm)
    bpy.ops.object.mode_set(mode="EDIT")
    eb = data.edit_bones
    for name, (h, t, parent) in BODY_BONES.items():
        b = eb.new(name)
        b.head, b.tail = Vector(h), Vector(t)
        b.roll = 0
        if parent:
            b.parent = eb[parent]
    for name, (pts, parent) in chain_bones().items():
        prev = eb[parent]
        n = len(pts) - 1
        for i in range(n + 1):
            b = eb.new(f"{name}_{i}")
            b.head = pts[i]
            # the last bone is a short tip, so every spring bone has a child
            b.tail = pts[i + 1] if i < n else pts[i] + (pts[i] - pts[i - 1]).normalized() * 0.05
            b.parent = prev
            b.use_connect = i > 0
            b.use_deform = i < n
            prev = b
    bpy.ops.object.mode_set(mode="OBJECT")
    return arm


# ───────────────────────── skinning ─────────────────────────

def seg_dist(p, a, b):
    ab = b - a
    t = max(0.0, min(1.0, (p - a).dot(ab) / ab.length_squared))
    return (p - (a + ab * t)).length


def body_weights(obj, arm, allowed=None):
    """Each vertex → its two nearest body bones, 1/d⁴ blended."""
    bones = [(b.name, b.head_local.copy(), b.tail_local.copy()) for b in arm.data.bones
             if b.name in DEFORM_BODY and (allowed is None or b.name in allowed)]
    groups = {n: obj.vertex_groups.new(name=n) for n, _, _ in bones}
    for v in obj.data.vertices:
        p = v.co
        # a limb never follows the other side's limb (inner thighs!)
        ds = sorted((seg_dist(p, h, t), n) for n, h, t in bones
                    if not (n.endswith("_L") and p.x < -0.01 or n.endswith("_R") and p.x > 0.01))
        near = ds[:2]
        ws = [(n, 1 / max(d, 1e-4) ** 4) for d, n in near]
        tot = sum(w for _, w in ws)
        for n, w in ws:
            groups[n].add([v.index], w / tot, "REPLACE")


def table_weights(obj, table):
    for i, ws in enumerate(table):
        tot = sum(ws.values()) or 1
        for n, w in ws.items():
            if w <= 1e-4:
                continue
            g = obj.vertex_groups.get(n) or obj.vertex_groups.new(name=n)
            g.add([i], w / tot, "REPLACE")


def bind(obj, arm):
    obj.parent = arm
    m = obj.modifiers.new("Rig", "ARMATURE")
    m.object = arm


# ───────────────────────── poses and actions ─────────────────────────

AX = {"x": Vector((1, 0, 0)), "y": Vector((0, 1, 0)), "z": Vector((0, 0, 1))}


def pose(arm, rots, hips_z=0.0):
    """rots: bone → [(axis, angle)], rotations about armature axes at that bone."""
    for pb in arm.pose.bones:
        pb.rotation_mode = "QUATERNION"
        pb.rotation_quaternion = Quaternion()
        pb.location = Vector()
    for name, lst in rots.items():
        pb = arm.pose.bones[name]
        rest = pb.bone.matrix_local.to_quaternion()
        q = Quaternion()
        for ax, ang in lst:
            q = Quaternion(AX[ax], ang) @ q
        pb.rotation_quaternion = rest.inverted() @ q @ rest
    hips = arm.pose.bones["hips"]
    hips.location = hips.bone.matrix_local.to_3x3().inverted() @ Vector((0, 0, hips_z))


def key_all(arm, frame):
    for name in BODY_BONES:
        pb = arm.pose.bones[name]
        pb.keyframe_insert("rotation_quaternion", frame=frame)
        if name == "hips":
            pb.keyframe_insert("location", frame=frame)


def arms_down(lst_l, lst_r, inward):
    return {"upper_arm_L": [("y", inward)] + lst_l, "upper_arm_R": [("y", -inward)] + lst_r}


def p_idle(t):
    ph = 2 * math.pi * t
    b = math.sin(ph)
    r = arms_down([("x", 0.04)], [("x", 0.02)], 0.40 + 0.02 * b)
    r.update({
        "chest": [("x", -0.015 * b)], "spine": [("x", 0.02)],
        "hips": [("z", 0.05), ("y", 0.025)],
        "thigh_L": [("y", 0.02), ("x", -0.03)], "thigh_R": [("y", 0.04), ("x", 0.04)],
        "shin_R": [("x", 0.10)],
        "forearm_L": [("x", -0.18)], "forearm_R": [("x", -0.12)],
        "hand_L": [("y", 0.2)], "hand_R": [("y", -0.2)],
        "neck": [("x", 0.04)], "head": [("z", -0.06 + 0.02 * math.sin(ph + 1)), ("y", 0.04)],
    })
    return r, 0.004 * b - 0.008


def p_run(t):
    ph = 2 * math.pi * t
    s, c = math.sin(ph), math.cos(ph)
    sw = 0.78
    r = arms_down([("x", 0.62 * s + 0.1)], [("x", -0.62 * s + 0.1)], 0.30)
    r.update({
        "thigh_L": [("x", -sw * s - 0.10)], "thigh_R": [("x", sw * s - 0.10)],
        "shin_L": [("x", 0.25 + 1.25 * max(0.0, c) ** 1.4)],
        "shin_R": [("x", 0.25 + 1.25 * max(0.0, -c) ** 1.4)],
        "foot_L": [("x", 0.25 * c)], "foot_R": [("x", -0.25 * c)],
        "forearm_L": [("x", -1.15 - 0.2 * s)], "forearm_R": [("x", -1.15 + 0.2 * s)],
        "spine": [("x", 0.14)], "chest": [("z", 0.16 * s), ("x", 0.04)],
        "hips": [("z", -0.10 * s)],
        "neck": [("z", -0.08 * s)], "head": [("x", -0.10)],
    })
    return r, 0.028 * math.cos(2 * ph) - 0.03


def p_glide(t):
    ph = 2 * math.pi * t
    b = math.sin(ph)
    r = {
        "upper_arm_L": [("y", -0.95 - 0.04 * b), ("x", 0.30)],
        "upper_arm_R": [("y", 0.95 + 0.04 * b), ("x", 0.30)],
        "forearm_L": [("x", -0.12), ("y", -0.10)], "forearm_R": [("x", -0.12), ("y", 0.10)],
        "hand_L": [("y", -0.3)], "hand_R": [("y", 0.3)],
        "thigh_L": [("x", 0.18)], "thigh_R": [("x", 0.10)],
        "shin_L": [("x", 0.35 + 0.05 * b)], "shin_R": [("x", 0.65 - 0.05 * b)],
        "foot_L": [("x", 0.5)], "foot_R": [("x", 0.5)],
        "spine": [("x", -0.18)], "chest": [("z", 0.05 * b), ("x", -0.06)],
        "neck": [("x", -0.25)], "head": [("x", -0.30)],
    }
    return r, 0.0


def p_hover(t):
    ph = 2 * math.pi * t
    b = math.sin(ph)
    r = arms_down([("x", -0.10)], [("x", -0.05)], -0.18 + 0.05 * b)
    r.update({
        "forearm_L": [("x", -0.35)], "forearm_R": [("x", -0.30)],
        "hand_L": [("y", -0.3)], "hand_R": [("y", 0.3)],
        "thigh_L": [("x", -0.30 + 0.05 * b)], "shin_L": [("x", 0.65)],
        "thigh_R": [("x", 0.08 - 0.04 * b)], "shin_R": [("x", 0.25)],
        "foot_L": [("x", 0.45)], "foot_R": [("x", 0.55)],
        "spine": [("x", 0.04)], "chest": [("x", -0.02 * b)],
        "head": [("x", 0.05), ("z", 0.04 * b)],
    })
    return r, 0.03 * b


def p_fly(t):
    ph = 2 * math.pi * t
    b = math.sin(ph)
    r = arms_down([("x", 0.22)], [("x", 0.22)], 0.42)
    r.update({
        "forearm_L": [("x", -0.10)], "forearm_R": [("x", -0.10)],
        "hand_L": [("y", 0.2)], "hand_R": [("y", -0.2)],
        "thigh_L": [("x", 0.10 + 0.04 * b)], "thigh_R": [("x", 0.04 - 0.04 * b)],
        "shin_L": [("x", 0.18)], "shin_R": [("x", 0.32)],
        "foot_L": [("x", 0.7)], "foot_R": [("x", 0.7)],
        "spine": [("x", -0.10)], "chest": [("z", 0.03 * b), ("x", -0.08)],
        "neck": [("x", -0.35)], "head": [("x", -0.40)],
    })
    return r, 0.0


def p_fall(t):
    ph = 2 * math.pi * t
    b = math.sin(ph)
    r = {
        "upper_arm_L": [("y", -0.55 - 0.06 * b), ("x", -0.25)],
        "upper_arm_R": [("y", 0.55 + 0.06 * b), ("x", -0.15)],
        "forearm_L": [("x", -0.45)], "forearm_R": [("x", -0.35)],
        "thigh_L": [("x", -0.55)], "shin_L": [("x", 1.0)],
        "thigh_R": [("x", 0.12)], "shin_R": [("x", 0.45)],
        "foot_L": [("x", 0.3)], "foot_R": [("x", 0.4)],
        "spine": [("x", 0.06)], "head": [("x", -0.06)],
    }
    return r, 0.0


ACTIONS = [("idle", p_idle, 72), ("run", p_run, 16), ("glide", p_glide, 48),
           ("hover", p_hover, 48), ("fly", p_fly, 40), ("fall", p_fall, 24)]


def build_actions(arm):
    arm.animation_data_create()
    made = []
    for name, fn, frames in ACTIONS:
        act = bpy.data.actions.new(name)
        act.use_fake_user = True
        arm.animation_data.action = act
        step = 2 if frames <= 24 else 4
        for f in list(range(0, frames, step)) + [frames]:
            rots, hz = fn((f % frames) / frames)
            pose(arm, rots, hz)
            key_all(arm, f)
        made.append(act)
    # each action on its own NLA track (muted) so the exporter finds them all
    for act in made:
        tr = arm.animation_data.nla_tracks.new()
        tr.name = act.name
        tr.strips.new(act.name, 0, act)
        tr.mute = True
    arm.animation_data.action = None
    pose(arm, {})


def check_poses(arm):
    """Sanity: in the run pose at t=.25 the left foot is ahead (−Y) of the right."""
    rots, hz = p_run(0.25)
    pose(arm, rots, hz)
    bpy.context.view_layer.update()
    fl = arm.pose.bones["foot_L"].head
    fr = arm.pose.bones["foot_R"].head
    hl = arm.pose.bones["hand_L"].head
    print(f"heroine: run t=.25 foot.L y={fl.y:.3f} foot.R y={fr.y:.3f} hand.L y={hl.y:.3f}")
    assert fl.y < fr.y, "left leg should swing forward"
    assert hl.y > 0, "left arm should swing back"
    rots, hz = p_glide(0)
    pose(arm, rots, hz)
    bpy.context.view_layer.update()
    w = arm.pose.bones["hand_L"].head
    print(f"heroine: glide hand.L z={w.z:.3f}")
    assert w.z > 1.25, "gliding arms should be spread up toward the shoulders"
    pose(arm, {})


# ───────────────────────── preview render ─────────────────────────

def render_preview(path):
    scn = bpy.context.scene
    scn.render.engine = "CYCLES"
    scn.cycles.device = "CPU"
    scn.cycles.samples = 24
    scn.render.resolution_x, scn.render.resolution_y = 900, 900
    # outlines, like the game's inverted hull
    scn.render.use_freestyle = True
    scn.render.line_thickness_mode = "ABSOLUTE"
    scn.render.line_thickness = 1.4
    ls = bpy.context.view_layer.freestyle_settings.linesets[0]
    ls.select_crease = False
    ls.select_border = False
    world = bpy.data.worlds.new("W")
    world.use_nodes = True
    world.node_tree.nodes["Background"].inputs["Color"].default_value = (0.75, 0.85, 1.0, 1)
    world.node_tree.nodes["Background"].inputs["Strength"].default_value = 0.9
    scn.world = world
    sun = link(bpy.data.objects.new("Sun", bpy.data.lights.new("Sun", "SUN")))
    sun.data.energy = 3.5
    sun.rotation_euler = (math.radians(50), math.radians(10), math.radians(-30))
    cam = link(bpy.data.objects.new("Cam", bpy.data.cameras.new("Cam")))
    cam.data.lens = 50
    scn.camera = cam
    arm = bpy.data.objects["Rig"]
    arm.animation_data.action = None
    for tr in arm.animation_data.nla_tracks:
        tr.mute = True
    views = [("front", (0.9, -3.4, 1.3), (0, 0, 1.0), p_idle(0)),
             ("back", (-1.6, 3.0, 1.3), (0, 0, 1.0), p_run(0.25)),
             ("side", (-3.4, -0.6, 1.2), (0, 0, 1.0), p_fly(0)),
             ("face", (0.18, -0.75, 1.72), (0, 0, 1.69), p_idle(0)),
             ("profile", (-0.62, -0.42, 1.71), (0, 0, 1.68), p_idle(0))]
    for tag, loc, at, (rots, hz) in views:
        pose(arm, rots, hz)
        cam.location = Vector(loc)
        d = Vector(at) - cam.location
        cam.rotation_euler = d.to_track_quat("-Z", "Y").to_euler()
        scn.render.filepath = path.replace(".png", f"-{tag}.png")
        bpy.ops.render.render(write_still=True)


# ───────────────────────── main ─────────────────────────

def main():
    clear_scene()
    arm = build_armature()
    body = build_body()
    body_weights(body, arm)
    buttons = build_buttons(body)
    body_weights(buttons, arm, allowed={"chest", "spine", "neck"})
    head = build_head()
    face = build_face()
    for o in (head, face):
        g = o.vertex_groups.new(name="head")
        g.add(list(range(len(o.data.vertices))), 1.0, "REPLACE")
    hair = build_hair()
    table_weights(hair, HAIR_WEIGHTS)
    coat = build_coat()
    table_weights(coat, HAIR_WEIGHTS_COAT)
    apply_modifiers(coat)
    collar = build_collar()
    apply_modifiers(collar)
    for v in collar.data.vertices:
        f = max(0.0, min(1.0, (v.co.z - 1.48) / 0.10))
        collar.vertex_groups.get("chest") or collar.vertex_groups.new(name="chest")
        collar.vertex_groups.get("neck") or collar.vertex_groups.new(name="neck")
        collar.vertex_groups["chest"].add([v.index], 1 - f, "REPLACE")
        collar.vertex_groups["neck"].add([v.index], f, "REPLACE")
    parts = [body, buttons, head, face, hair, coat, collar]
    for o in parts:
        bind(o, arm)
    activate(body)
    for o in parts:
        o.select_set(True)
    bpy.ops.object.join()
    # the joined mesh takes the body's (absent) UVs as the render default
    body.data.uv_layers["UVMap"].active_render = True
    body.name = "Heroine"
    body.data.name = "Heroine"
    check_poses(arm)
    build_actions(arm)
    print(f"heroine: {len(body.data.vertices)} verts, {len(body.data.polygons)} faces, "
          f"{len(arm.data.bones)} bones, {len(bpy.data.actions)} actions")
    bpy.ops.export_scene.gltf(
        filepath=OUT, export_format="GLB", export_yup=True, export_apply=False,
        export_animations=True, export_animation_mode="NLA_TRACKS", export_skins=True,
        export_def_bones=False, export_force_sampling=True, export_frame_step=1,
        export_morph=False, export_materials="EXPORT", export_image_format="AUTO",
        export_texcoords=True, export_normals=True, export_cameras=False, export_lights=False)
    print(f"heroine: wrote {OUT}")
    if PREVIEW:
        render_preview(PREVIEW)


if __name__ == "__main__":
    main()
