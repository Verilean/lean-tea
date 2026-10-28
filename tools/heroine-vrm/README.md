# heroine-vrm — how `examples/Breezehollow/heroine.glb` is made

The heroine is put together from two freely licensed sample avatars
(VRM 0.x files, which are glTF with extensions), recoloured toward her
character sheet and given the game's skeleton and clips.
Everything runs in Blender on the CPU; run it from the repository root.

| Step | Script | What it does |
|---|---|---|
| 1. Combine, recolour | `build.py` | Body, face and dress of one avatar, the long straight hair of the other; silver hair, violet eyes, white and violet dress |
| 2. Rig, export | `rig.py` | heroine.py's skeleton and clips, hair and skirt on the spring chains |

```bash
$ mkdir -p /tmp/hv && cd /tmp/hv
$ curl -LO https://opengameart.org/sites/default/files/avatarsample_g.zip
$ curl -LO https://opengameart.org/sites/default/files/sendagaya_shino.zip
$ unzip -o avatarsample_g.zip && unzip -o sendagaya_shino.zip && cd -

$ blender -b --factory-startup --python tools/heroine-vrm/build.py -- \
    /tmp/hv/AvatarSample_G.vrm "/tmp/hv/Sendagaya Shino.vrm" /tmp/hv/heroine.blend
$ blender -b --factory-startup --python tools/heroine-vrm/rig.py -- \
    /tmp/hv/heroine.blend examples/Breezehollow/heroine.glb --preview /tmp/hv/rig.png
```

## Notes

- **Hair swap:** the hair mesh and its bones move from one avatar to the
  other by the offset between the two head bones; both avatars share the
  same base head, so it fits without further adjustment.
- **Recolouring** works on the textures: hair lightness is stretched over
  its range and mapped from a lavender shadow to near white, pink and
  teal in the dress become violet, the iris hue is turned to violet. The
  face parts (brows, lashes, eye lines) are tinted through their material
  colour.
- **Rig:** the arms are lowered into the rest pose `heroine.py`'s clips
  were made for and the pose is baked into the meshes. Each vertex keeps
  the avatar's own weights, renamed to the matching game bone; weights on
  the avatar's hair and skirt bones are handed to the game's `hair_*`,
  `lock_*` and `coat_*` chains by position.
- **Materials** are named for `World.leanjs`, which tells them apart up
  to the first dot: `Face`, `Skin`, `Hair`, `Eye` (eyes, brows, lashes,
  mouth: drawn flat, no outline) and `Cloth`.

## Sources and licences

| Used for | Source | Licence |
|---|---|---|
| Body, face, dress | `AvatarSample_G.vrm`, a sample avatar of the VRoid project (alpha release) | Embedded VRM licence: anyone may use, modify and redistribute it, commercially too, no credit required |
| Hair | `Sendagaya Shino.vrm`, a sample avatar of the VRoid project (alpha release) | Same as above |

Both files are from the collection at
<https://opengameart.org/node/170868>, which lists the alpha-release
samples as released under CC0 (it leaves out the ones with stricter
terms). Only the derived `heroine.glb` is in the repository.
