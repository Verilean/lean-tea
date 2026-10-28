"""Give the heroine from build.py the game's skeleton and clips.

    blender -b --factory-startup --python tools/heroine-vrm/rig.py -- \
        IN.blend examples/Breezehollow/heroine.glb [--preview out.png]

The skeleton and clips are tools/blender/heroine.py's, so World.leanjs's
bone names, spring chains and colliders carry over:

  * The arms are lowered into heroine.py's rest pose (upper arms 30
    degrees off vertical), the pose is baked into the meshes and she is
    turned to face -Y, 1.8 m from soles to crown.
  * heroine.py's body bones are put on the avatar's joints. Each vertex
    keeps the avatar's weights, renamed to the matching body bone.
  * Weights on the avatar's hair bones go to the hair_* and lock_* chains
    by height and side; weights on its skirt bones go to the coat_* chains
    by angle round the hips.
"""

import importlib.util
import math
import os
import sys

import bpy
import numpy as np
from mathutils import Matrix, Vector

here = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location("heroine", os.path.join(here, "..", "blender", "heroine.py"))
H = importlib.util.module_from_spec(spec)
spec.loader.exec_module(H)

argv = sys.argv[sys.argv.index("--") + 1:]
SRC, OUT = argv[0], argv[1]
PREVIEW = argv[argv.index("--preview") + 1] if "--preview" in argv else None

HEIGHT = 1.80                 # soles .. crown of the head (hair excluded)
ARM_BONES = ("UpperArm", "LowerArm", "Hand", "Sleeve", "Thumb", "Index", "Middle", "Ring", "Little", "Shoulder")


# ───────────────────────── pose and placement ─────────────────────────

def lower_arms(arm):
    def pos(n, tail=False):
        pb = arm.pose.bones[n]
        return arm.matrix_world @ (pb.tail if tail else pb.head)

    def turn(n, ang):
        pb = arm.pose.bones[n]
        h = pb.head.copy()
        pb.matrix = Matrix.Translation(h) @ Matrix.Rotation(ang, 4, "Y") @ Matrix.Translation(-h) @ pb.matrix
        bpy.context.view_layer.update()

    for side in "LR":
        for bone, child, droop in (("UpperArm", "LowerArm", 60), ("LowerArm", "Hand", 66)):
            d = pos(f"J_Bip_{side}_{child}") - pos(f"J_Bip_{side}_{bone}")
            s = 1 if d.x > 0 else -1
            turn(f"J_Bip_{side}_{bone}", s * (math.radians(droop) - math.atan2(-d.z, abs(d.x))))
    names = {"hips": "Hips", "spine": "Spine", "chest": "Chest", "neck": "Neck", "head": "Head"}
    J = {k: pos(f"J_Bip_C_{v}") for k, v in names.items()}
    for side in "LR":
        for k, v in (("clav", "Shoulder"), ("sh", "UpperArm"), ("elb", "LowerArm"), ("wri", "Hand"),
                     ("hnd", "Middle1"), ("hip", "UpperLeg"), ("knee", "LowerLeg"), ("ank", "Foot")):
            J[f"{k}_{side}"] = pos(f"J_Bip_{side}_{v}")
        J[f"toe_{side}"] = pos(f"J_Bip_{side}_ToeBase", tail=True)
    return J


def bake_pose(meshes, arm):
    for o in meshes:
        bpy.context.view_layer.objects.active = o
        if o.data.shape_keys:
            o.shape_key_clear()
        for m in list(o.modifiers):
            if m.type == "ARMATURE":
                bpy.ops.object.modifier_apply(modifier=m.name)
        mw = o.matrix_world.copy()
        o.parent = None
        o.matrix_world = mw
        for x in bpy.context.view_layer.objects:
            x.select_set(x is o)
        bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    bpy.data.objects.remove(arm)


def place(meshes, J):
    """Face -Y, soles at 0, crown at HEIGHT, the head's centre on the z axis."""
    face = next(o for o in meshes if o.name.startswith("Face"))
    fco = np.array([v.co[:] for v in face.data.vertices])
    sole = min(v.co.z for o in meshes if not o.name.startswith("Hair") for v in o.data.vertices)
    k = HEIGHT / (fco[:, 2].max() - sole)
    cx, cy = (fco[:, 0].min() + fco[:, 0].max()) / 2, (fco[:, 1].min() + fco[:, 1].max()) / 2
    M = Matrix.Rotation(math.pi, 4, "Z") @ Matrix.Scale(k, 4) @ Matrix.Translation((-cx, -cy, -sole))
    for o in meshes:
        o.data.transform(M)
        o.data.update()
    J = {n: M @ p for n, p in J.items()}
    assert J["sh_L"].x > 0, "her left should be +x"
    return J


def body_bones(J):
    def c(v):
        return (0, v.y, v.z)
    bones = {
        "root":  ((0, 0, 0), (0, 0, 0.15), None),
        "hips":  (c(J["hips"]), c(J["spine"]), "root"),
        "spine": (c(J["spine"]), c(J["chest"]), "hips"),
        "chest": (c(J["chest"]), c(J["neck"]), "spine"),
        "neck":  (c(J["neck"]), c(J["head"]), "chest"),
        "head":  (c(J["head"]), (0, J["head"].y, HEIGHT), "neck"),
    }
    for s in "LR":
        hand_tip = J[f"hnd_{s}"] + (J[f"hnd_{s}"] - J[f"wri_{s}"]) * 0.6
        bones.update({
            f"shoulder_{s}":  (tuple(J[f"clav_{s}"]), tuple(J[f"sh_{s}"]), "chest"),
            f"upper_arm_{s}": (tuple(J[f"sh_{s}"]), tuple(J[f"elb_{s}"]), f"shoulder_{s}"),
            f"forearm_{s}":   (tuple(J[f"elb_{s}"]), tuple(J[f"wri_{s}"]), f"upper_arm_{s}"),
            f"hand_{s}":      (tuple(J[f"wri_{s}"]), tuple(hand_tip), f"forearm_{s}"),
            f"thigh_{s}":     (tuple(J[f"hip_{s}"]), tuple(J[f"knee_{s}"]), "hips"),
            f"shin_{s}":      (tuple(J[f"knee_{s}"]), tuple(J[f"ank_{s}"]), f"thigh_{s}"),
            f"foot_{s}":      (tuple(J[f"ank_{s}"]), tuple(J[f"toe_{s}"]), f"shin_{s}"),
        })
    return bones


def body_bone(n):
    """The game's body bone for one of the avatar's (None: hair or skirt)."""
    if "HairJoint" in n or "Skirt" in n:
        return None
    side = "L" if "_L_" in n else "R" if "_R_" in n else None
    if side is None:
        for key, bone in (("Hips", "hips"), ("Chest", "chest"), ("Neck", "neck"), ("Head", "head"), ("Face", "head")):
            if key in n:
                return bone
        return "spine" if n.endswith("Spine") else "hips"
    for keys, bone in ((("Bust",), "chest"), (("Eye",), "head"), (("Shoulder",), "shoulder"),
                       (("UpperArm",), "upper_arm"), (("LowerArm", "Sleeve"), "forearm"),
                       (("Hand", "Thumb", "Index", "Middle", "Ring", "Little"), "hand"),
                       (("UpperLeg",), "thigh"), (("LowerLeg",), "shin"), (("Foot", "Toe"), "foot")):
        if any(k in n for k in keys):
            return bone if bone in ("chest", "head") else f"{bone}_{side}"
    raise ValueError(n)


# ───────────────────────── the skirt ─────────────────────────

class Skirt:
    """Where the dress is, round the hips."""

    def __init__(self, meshes, weights, J):
        # the dress and legs, without the arms hanging beside them
        self.pts = np.array([v.co[:] for o in meshes if o.name.startswith("Body")
                             for v, ws in zip(o.data.vertices, weights[o.name])
                             if sum(w for n, w in ws.items() if any(a in n for a in ARM_BONES)) < 0.1])
        self.y = J["hips"].y

    def radius(self, th, z):
        """How far the dress reaches from the hips' axis toward angle th at height z."""
        d = np.array([math.sin(th), -math.cos(th)])
        rel = self.pts[:, :2] - np.array([0, self.y])
        m = (np.abs(self.pts[:, 2] - z) < 0.03) & (rel @ d > 0.9 * np.linalg.norm(rel, axis=1))
        return np.linalg.norm(rel[m], axis=1).max() if m.any() else 0.0


# ───────────────────────── chains and weights ─────────────────────────

def coat_weights(p, top_z, hem_z, hips_y, w=1.0):
    """The two nearest coat chains by angle, two bones along them by depth."""
    th = math.atan2(p.x, -(p.y - hips_y))
    s = max(0.0, min(1.0, (top_z - p.z) / (top_z - hem_z)))
    c = min(max(s * 3 - 0.5, 0.0), 2)
    lo = int(c)
    hi = min(lo + 1, 2)
    f = c - lo
    out = {}
    for name, wa in H.angle_weights(th):
        for idx, wb in ((lo, 1 - f), (hi, f)):
            out[f"{name}_{idx}"] = out.get(f"{name}_{idx}", 0) + w * wa * wb
    return out


def main():
    bpy.ops.wm.open_mainfile(filepath=SRC)
    avatar = next(o for o in bpy.data.objects if o.type == "ARMATURE")
    meshes = [o for o in bpy.data.objects if o.type == "MESH"]
    J = lower_arms(avatar)
    bake_pose(meshes, avatar)
    J = place(meshes, J)
    chin = J["head"].z

    weights = {o.name: [{o.vertex_groups[g.group].name: g.weight for g in v.groups if g.weight > 1e-4}
                        for v in o.data.vertices] for o in meshes}
    hair_pts, dress_pts = [], []
    for o in meshes:
        for v, ws in zip(o.data.vertices, weights[o.name]):
            if sum(w for n, w in ws.items() if "HairJoint" in n) > 0.5:
                hair_pts.append(v.co[:])
            if sum(w for n, w in ws.items() if "Skirt" in n) > 0.3:
                dress_pts.append(v.co[:])
    hair_pts, dress_pts = np.array(hair_pts), np.array(dress_pts)
    top_z = J["hips"].z + 0.06
    skirt = Skirt(meshes, weights, J)
    hem_z = float(np.percentile(dress_pts[:, 2], 2))

    # spring chains: three down the back hair, a lock in front of each
    # shoulder, five round the skirt
    back_levels = list(np.linspace(chin, hair_pts[:, 2].min() + 0.04, 5))
    lock_levels = list(np.linspace(chin, max(chin - 0.45, hair_pts[:, 2].min() + 0.05), 4))
    back_chains = [("hair_R", -0.08), ("hair_C", 0.0), ("hair_L", 0.08)]

    def hair_y(x, z, front):
        m = (np.abs(hair_pts[:, 2] - z) < 0.04) & (np.abs(hair_pts[:, 0] - x) < 0.05)
        m &= (hair_pts[:, 1] < -0.02) if front else (hair_pts[:, 1] > 0)
        if not m.any():
            return -0.06 if front else 0.12
        return hair_pts[m, 1].mean() if front else hair_pts[m, 1].max() - 0.02

    chains = {}
    for n, x in back_chains:
        chains[n] = ([Vector((x * (1 + 0.1 * i), hair_y(x, z, False), z)) for i, z in enumerate(back_levels)], "head")
    for s, n in ((1, "lock_L"), (-1, "lock_R")):
        chains[n] = ([Vector((s * 0.11, hair_y(s * 0.11, z, True), z)) for z in lock_levels], "head")
    for n, th in H.COAT_CHAINS:
        pts = []
        for i in range(4):
            z = top_z + (hem_z - top_z) * i / 3
            r = 0.9 * max(skirt.radius(th, z), 0.12)
            pts.append(Vector((math.sin(th) * r, J["hips"].y - math.cos(th) * r, z)))
        chains[n] = (pts, "hips")

    H.BODY_BONES = body_bones(J)
    H.DEFORM_BODY = [b for b in H.BODY_BONES if b != "root"]
    H.chain_bones = lambda: chains
    arm = H.build_armature()

    for o in meshes:
        table = []
        for v, ws in zip(o.data.vertices, weights[o.name]):
            p, out = v.co, {}

            def add(d, k=1.0):
                for n, w in d.items():
                    out[n] = out.get(n, 0) + w * k

            add({body_bone(n): w for n, w in ws.items() if body_bone(n)})
            h = sum(w for n, w in ws.items() if "HairJoint" in n)
            if h > 0:
                if p.y < -0.02 and p.z < chin:
                    add(H.chain_weights(p, [("lock_L" if p.x > 0 else "lock_R", 0.0)], lock_levels, chin - 0.02), h)
                else:
                    add(H.chain_weights(p, back_chains, back_levels, chin - 0.02), h)
            k = sum(w for n, w in ws.items() if "Skirt" in n)
            if k > 0:
                add(coat_weights(p, top_z, hem_z, J["hips"].y, k))
            table.append(out or {"hips": 1.0})
        for g in list(o.vertex_groups):
            o.vertex_groups.remove(g)
        H.table_weights(o, table)
        H.activate(o)
        bpy.ops.object.mode_set(mode="WEIGHT_PAINT")
        bpy.ops.object.vertex_group_limit_total(group_select_mode="ALL", limit=4)
        bpy.ops.object.vertex_group_normalize_all(group_select_mode="ALL", lock_active=False)
        bpy.ops.object.mode_set(mode="OBJECT")
        H.bind(o, arm)

    # material names World.leanjs tells apart (up to the first dot)
    for m in bpy.data.materials:
        if not m.users:
            continue
        n = m.name
        m.name = ("Face" if "Face_00_SKIN" in n
                  else "Skin" if "Body_00_SKIN" in n else "Hair" if "HAIR" in n
                  else "Eye" if any(t in n for t in ("Eye", "Brow", "Mouth")) else "Cloth")

    H.check_poses(arm)
    H.build_actions(arm)
    for x in bpy.context.view_layer.objects:
        x.select_set(x is arm or x in meshes)
    bpy.ops.export_scene.gltf(
        filepath=OUT, export_format="GLB", export_yup=True, export_apply=False, use_selection=True,
        export_animations=True, export_animation_mode="NLA_TRACKS", export_skins=True,
        export_def_bones=False, export_force_sampling=True, export_frame_step=1,
        export_morph=False, export_materials="EXPORT", export_image_format="AUTO",
        export_texcoords=True, export_normals=True, export_cameras=False, export_lights=False)
    print(f"rig: skirt hem at {hem_z:.2f} m, {len(arm.data.bones)} bones, {len(bpy.data.actions)} actions, wrote {OUT}")
    if PREVIEW:
        preview(arm, PREVIEW)


def preview(arm, path):
    """Flat-shaded orthographic shots in a few clip poses, side by side."""
    for m in bpy.data.materials:
        nt = m.node_tree
        bsdf = next((n for n in nt.nodes if n.type == "BSDF_PRINCIPLED"), None) if nt else None
        out = next((n for n in nt.nodes if n.type == "OUTPUT_MATERIAL"), None) if nt else None
        if not bsdf or not out:
            continue
        em, tr, mix = (nt.nodes.new(t) for t in ("ShaderNodeEmission", "ShaderNodeBsdfTransparent", "ShaderNodeMixShader"))
        for src, dst in ((bsdf.inputs["Base Color"], em.inputs["Color"]), (bsdf.inputs["Alpha"], mix.inputs[0])):
            if src.links:
                nt.links.new(src.links[0].from_socket, dst)
            else:
                dst.default_value = src.default_value
        nt.links.new(tr.outputs[0], mix.inputs[1])
        nt.links.new(em.outputs[0], mix.inputs[2])
        nt.links.new(mix.outputs[0], out.inputs["Surface"])
    sc = bpy.context.scene
    sc.render.engine = "CYCLES"
    sc.cycles.samples = 16
    sc.cycles.use_denoising = False
    sc.cycles.max_bounces = 0
    sc.cycles.transparent_max_bounces = 32
    sc.render.resolution_x, sc.render.resolution_y = 500, 700
    sc.world = bpy.data.worlds.new("W")
    sc.world.color = (0.55, 0.57, 0.6)
    cam = H.link(bpy.data.objects.new("Cam", bpy.data.cameras.new("Cam")))
    cam.data.type = "ORTHO"
    cam.data.ortho_scale = 2.1
    sc.camera = cam
    for tr in arm.animation_data.nla_tracks:
        tr.mute = True
    for i, (act, frame, ang) in enumerate([("idle", 6, 0), ("idle", 6, 35), ("run", 4, 90), ("glide", 10, 20), ("idle", 6, 180)]):
        arm.animation_data.action = bpy.data.actions[act]
        sc.frame_set(frame)
        r = math.radians(ang)
        d = Vector((math.sin(r), -math.cos(r), 0.15)).normalized()
        cam.location = Vector((0, 0, 0.95)) + d * 5
        cam.rotation_euler = (-d).to_track_quat("-Z", "Y").to_euler()
        sc.render.filepath = path.replace(".png", f"-{i}.png")
        bpy.ops.render.render(write_still=True)


main()
