"""BrickArchitect-style line-art renderer for LDraw parts (local LDraw library version).
White faces, light-grey shading by face orientation, black edge + silhouette lines; z-buffered.
Usage: python3 render_ldraw_lineart.py icon_map.csv      (cols: subtype, ldraw, colour, icon)"""
import os, sys, csv, math, re
import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage

LDRAW = os.environ.get("LDRAW_DIR", "/Users/pedro/Documentos/LEGO Creations/Utils/ldraw")
_cache = {}
def fetch(name):
    n = name.lower().replace("\\", "/")
    if n in _cache: return _cache[n]
    txt = None
    for sub in ("parts", "p", "parts/s"):
        f = os.path.join(LDRAW, sub, n)
        if os.path.isfile(f):
            txt = open(f, encoding="latin-1").read(); break
    _cache[n] = txt
    return txt
def prefetch(_): pass

PRIM_RE = re.compile(r"^\d+-\d+[a-z]")
def hires(name):
    n = name.lower().replace("\\", "/")
    if "/" not in n and PRIM_RE.match(n) and fetch("48/" + n): return "48/" + n
    return name

OUT = "icons_lineart"; os.makedirs(OUT, exist_ok=True)

def collect(name, M, tris, lines, depth=0, in_stud=False):
    txt = fetch(name)
    if not txt or depth > 12: return
    for ln in txt.splitlines():
        t = ln.split()
        if not t: continue
        typ = t[0]
        if typ == "1" and len(t) >= 15:
            v = list(map(float, t[2:14]))
            m = np.array([[v[3], v[4], v[5], v[0]], [v[6], v[7], v[8], v[1]],
                          [v[9], v[10], v[11], v[2]], [0, 0, 0, 1]])
            sub = hires(" ".join(t[14:]))
            stud = in_stud or os.path.basename(sub.lower().replace("\\", "/")).startswith("stud")
            collect(sub, M @ m, tris, lines, depth + 1, stud)
            continue
        if typ not in ("2", "3", "4"): continue
        k = {"2": 2, "3": 3, "4": 4}[typ]
        try: p = np.array(list(map(float, t[2:2 + 3 * k]))).reshape(k, 3)
        except ValueError: continue
        p = (M[:3, :3] @ p.T).T + M[:3, 3]
        if typ == "2": lines.append((p, in_stud))
        elif typ == "3": tris.append(p)
        else: tris.append(p[[0, 1, 2]]); tris.append(p[[0, 2, 3]])

def view_matrix():
    yaw, pitch = math.radians(-35), math.radians(-28)
    Ry = np.array([[math.cos(yaw), 0, math.sin(yaw)], [0, 1, 0], [-math.sin(yaw), 0, math.cos(yaw)]])
    Rx = np.array([[1, 0, 0], [0, math.cos(pitch), -math.sin(pitch)], [0, math.sin(pitch), math.cos(pitch)]])
    return Rx @ Ry @ np.diag([1, -1, 1])

def render(part, out_png, S=640, ss=4, line_px=2.2):
    tris, lines = [], []
    collect(part + ".dat", np.eye(4), tris, lines)
    if not tris: return False
    V = view_matrix()
    T = np.array([V @ t.T for t in tris]).transpose(0, 2, 1)
    allp = T.reshape(-1, 3)
    lo, hi = allp[:, :2].min(0), allp[:, :2].max(0)
    N = S * ss; pad = 0.06 * N
    scale = (N - 2 * pad) / (hi - lo).max()
    off = (N - (hi - lo) * scale) / 2
    def to_px(q):
        x = (q[..., 0] - lo[0]) * scale + off[0]
        y = N - ((q[..., 1] - lo[1]) * scale + off[1])
        return np.stack([x, y, q[..., 2] * scale], -1)
    P = to_px(T)
    zbuf = np.full((N, N), np.inf); shade = np.ones((N, N))
    light = np.array([-0.35, 0.85, -0.4]); light /= np.linalg.norm(light)
    for tri3d, tri in zip(T, P):
        n = np.cross(tri3d[1] - tri3d[0], tri3d[2] - tri3d[0]); nn = np.linalg.norm(n)
        if nn == 0: continue
        n /= nn
        if n[2] > 0: n = -n
        lam = max(0.0, n @ light)
        g = 0.84 + 0.16 * lam
        g = 1.0 if lam > 0.8 else g
        x0, y0 = np.floor(tri[:, :2].min(0)).astype(int); x1, y1 = np.ceil(tri[:, :2].max(0)).astype(int)
        x0, y0 = max(x0, 0), max(y0, 0); x1, y1 = min(x1, N - 1), min(y1, N - 1)
        if x1 < x0 or y1 < y0: continue
        xs, ys = np.meshgrid(np.arange(x0, x1 + 1) + 0.5, np.arange(y0, y1 + 1) + 0.5)
        (ax, ay, az), (bx, by, bz), (cx, cy, cz) = tri
        den = (by - cy) * (ax - cx) + (cx - bx) * (ay - cy)
        if abs(den) < 1e-9: continue
        w0 = ((by - cy) * (xs - cx) + (cx - bx) * (ys - cy)) / den
        w1 = ((cy - ay) * (xs - cx) + (ax - cx) * (ys - cy)) / den
        w2 = 1 - w0 - w1
        inside = (w0 >= -1e-6) & (w1 >= -1e-6) & (w2 >= -1e-6)
        if not inside.any(): continue
        z = w0 * az + w1 * bz + w2 * cz
        sub = zbuf[y0:y1 + 1, x0:x1 + 1]
        upd = inside & (z < sub)
        sub[upd] = z[upd]
        shade[y0:y1 + 1, x0:x1 + 1][upd] = g

    ink_img = Image.new("L", (N, N), 0)
    pen = ImageDraw.Draw(ink_img)
    eps = 0.012 * N
    width = max(1, int(round(line_px * ss)))
    def draw(seg):
        a, b = seg
        L = np.hypot(*(b[:2] - a[:2])); k = max(2, int(L))
        t = np.linspace(0, 1, k)[:, None]; pts = a + (b - a) * t
        xi = np.clip(pts[:, 0].astype(int), 0, N - 1); yi = np.clip(pts[:, 1].astype(int), 0, N - 1)
        zl = zbuf[yi, xi]
        for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            zl = np.maximum(zl, zbuf[np.clip(yi + dy, 0, N - 1), np.clip(xi + dx, 0, N - 1)])
        vis = pts[:, 2] <= zl + eps
        i = 0
        while i < k:
            if not vis[i]: i += 1; continue
            j = i
            while j + 1 < k and vis[j + 1]: j += 1
            if j - i >= min(3 * width, k - 1):
                pen.line([tuple(pts[i, :2]), tuple(pts[j, :2])], fill=255, width=width)
            i = j + 1
        for p, v in ((a, vis[:3 * width].all()), (b, vis[-3 * width:].all())):
            if v:
                rr = width / 2
                pen.ellipse([p[0] - rr, p[1] - rr, p[0] + rr, p[1] + rr], fill=255)
    n_stud = sum(st for _, st in lines)
    skip_studs = n_stud > 4000
    for ln, st in lines:
        if skip_studs and st: continue
        draw(to_px((V @ ln.T).T))
    finite = np.isfinite(zbuf)
    far = np.where(finite, zbuf, zbuf[finite].max() + 1e6)
    thr = 1.5 * scale
    if skip_studs: thr = far.max() - far[finite].min() + 1
    big = far.max() + 1e6
    Pd = np.pad(far, 1, constant_values=big)
    sil = np.zeros((N, N), bool)
    for dy in (-1, 0, 1):
        for dx in (-1, 0, 1):
            if dy == dx == 0: continue
            nb = Pd[1 + dy:1 + dy + N, 1 + dx:1 + dx + N]
            sil |= finite & (nb - far > thr)
    rad = max(1, int(round(width / 2)))
    yy, xx = np.mgrid[-rad:rad + 1, -rad:rad + 1]
    sil = ndimage.binary_dilation(sil, structure=(xx ** 2 + yy ** 2) <= rad ** 2)
    ink_img = Image.fromarray(np.maximum(np.asarray(ink_img), sil.astype(np.uint8) * 255))
    ink = np.asarray(ink_img, dtype=float) / 255
    filled = np.isfinite(zbuf).astype(float)
    grey = shade * (1 - ink)
    rgba = np.zeros((N, N, 4), np.uint8)
    rgba[..., :3] = (np.clip(grey, 0, 1)[..., None] * 255).astype(np.uint8)
    rgba[..., 3] = (np.maximum(filled, ink) * 255).astype(np.uint8)
    Image.fromarray(rgba, "RGBA").resize((S, S), Image.LANCZOS).save(out_png)
    return True

if __name__ == "__main__":
    rows = list(csv.DictReader(open(sys.argv[1])))
    for r in rows:
        ok = False
        for part in r["ldraw"].split("|"):
            ok = render(part, os.path.join(OUT, r["icon"] + ".png"))
            if ok: break
        print(f'{r["subtype"]:45s} {"ok" if ok else "MISSING"}', flush=True)
