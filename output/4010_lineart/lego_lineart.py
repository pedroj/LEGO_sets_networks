#!/usr/bin/env python3
"""
lego_lineart.py - BrickArchitect-style line-art icons for LEGO parts, from LDraw geometry.
Usage: python3 lego_lineart.py --parts parts.txt|adjacency.csv --out icons/ [--ldraw-dir DIR] [--size 640] [--jobs 4] [--force]
Writes icons/<part>.png (transparent RGBA) and icons/manifest.csv
  (part_num, ldraw_file, description, ldraw_category, icon, status)
"""
import os, sys, re, csv, math, argparse, urllib.request
import concurrent.futures as cf
import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage

MIRROR = "https://raw.githubusercontent.com/gkjohnson/ldraw-parts-library/master/complete/ldraw/"
CACHE = os.environ.get("LDRAW_CACHE", os.path.expanduser("~/.cache/ldraw_lineart"))
LOCAL = None
PRIM_RE = re.compile(r"^\d+-\d+[a-z]")

def fetch(name):
    name = name.lower().replace("\\", "/").strip()
    os.makedirs(CACHE, exist_ok=True)
    if LOCAL:
        for prefix in ("parts/", "p/", "Parts/", "P/"):
            f = os.path.join(LOCAL, prefix + name)
            if os.path.exists(f):
                return open(f, encoding="latin-1").read()
    local = os.path.join(CACHE, name.replace("/", "__"))
    if os.path.exists(local):
        return open(local, encoding="latin-1").read() or None
    for prefix in ("parts/", "p/"):
        try:
            txt = urllib.request.urlopen(MIRROR + prefix + name, timeout=20).read().decode("latin-1")
            open(local, "w", encoding="latin-1").write(txt)
            return txt
        except Exception:
            pass
    open(local, "w").write("")
    return None

def refs(txt):
    return [" ".join(t[14:]) for t in (l.split() for l in txt.splitlines()) if len(t) >= 15 and t[0] == "1"]

def prefetch(root):
    seen, frontier = set(), [root]
    with cf.ThreadPoolExecutor(16) as ex:
        while frontier:
            frontier = [f for f in frontier if f.lower() not in seen]
            for f in frontier: seen.add(f.lower())
            frontier = [r for t in ex.map(fetch, frontier) if t for r in refs(t)]

def hires(name):
    n = name.lower().replace("\\", "/")
    if "/" not in n and PRIM_RE.match(n) and fetch("48/" + n):
        return "48/" + n
    return name

PRINT_RE = re.compile(r"(pr|pat|ps|px|pb|pc|pt|pq|d|dm)\d+[a-z0-9]*$")

def candidates(p):
    p = p.strip().lower()
    out = [p]
    base = PRINT_RE.sub("", p) if not re.fullmatch(r"\d+", p) else p
    out += [base]
    stripped = re.sub(r"[a-z]+$", "", base)
    out += [base + s for s in ("b", "a", "c", "d")] + [stripped] + [stripped + s for s in ("b", "a", "c")]
    seen = []
    for c in out:
        if c and c not in seen: seen.append(c)
    return seen

def header(txt):
    desc, cat = "", ""
    for ln in txt.splitlines():
        t = ln.strip()
        if not desc and t.startswith("0 "):
            desc = t[2:].strip()
        if t.startswith("0 !CATEGORY"):
            cat = t[len("0 !CATEGORY"):].strip()
    return re.sub(r"\s+", " ", desc), cat

def resolve(part):
    for c in candidates(part):
        txt = fetch(c + ".dat")
        if not txt: continue
        desc, cat = header(txt)
        m = re.match(r"~moved to\s+(\S+)", desc, re.I)
        if m:
            tgt = m.group(1).lower().removesuffix(".dat")
            t2 = fetch(tgt + ".dat")
            if t2:
                desc, cat = header(t2); c = tgt
        if not cat:
            cat = re.sub(r"^[~=_|]+", "", desc).split(" ")[0] if desc else ""
        return c, desc, cat
    return None, "", ""

def collect(name, M, tris, lines, depth=0, in_stud=False):
    txt = fetch(name)
    if not txt or depth > 14: return
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
        k = int(typ)
        try:
            p = np.array(list(map(float, t[2:2 + 3 * k]))).reshape(k, 3)
        except ValueError:
            continue
        p = (M[:3, :3] @ p.T).T + M[:3, 3]
        if typ == "2": lines.append((p, in_stud))
        elif typ == "3": tris.append(p)
        else: tris.append(p[[0, 1, 2]]); tris.append(p[[0, 2, 3]])

def view_matrix(yaw_deg=-35, pitch_deg=-28):
    yaw, pitch = math.radians(yaw_deg), math.radians(pitch_deg)
    Ry = np.array([[math.cos(yaw), 0, math.sin(yaw)], [0, 1, 0], [-math.sin(yaw), 0, math.cos(yaw)]])
    Rx = np.array([[1, 0, 0], [0, math.cos(pitch), -math.sin(pitch)], [0, math.sin(pitch), math.cos(pitch)]])
    return Rx @ Ry @ np.diag([1, -1, 1])

STYLE = "lineart-circlefit-v2"

def enclosing_circle(xy):
    try:
        from scipy.spatial import ConvexHull
        xy = xy[ConvexHull(xy).vertices]
    except Exception:
        pass
    p = xy[0]; q = xy[np.argmax(((xy - p) ** 2).sum(1))]; r_ = xy[np.argmax(((xy - q) ** 2).sum(1))]
    c = (q + r_) / 2; R = np.linalg.norm(q - r_) / 2
    for _ in range(3):
        for pt in xy:
            d = np.linalg.norm(pt - c)
            if d > R:
                R = (R + d) / 2; c = c + (d - R) / d * (pt - c)
    for _ in range(60):
        d = np.sqrt(((xy - c) ** 2).sum(1)); far = xy[np.argmax(d)]
        c2 = c + 0.02 * (far - c); R2 = np.sqrt(((xy - c2) ** 2).sum(1)).max()
        if R2 < d.max() - 1e-9: c = c2
        else: break
    return c, np.sqrt(((xy - c) ** 2).sum(1)).max()

def render(part_file, out_png, S=640, ss=4, line_px=2.2):
    prefetch(part_file + ".dat")
    tris, lines = [], []
    collect(part_file + ".dat", np.eye(4), tris, lines)
    if not tris: return False
    V = view_matrix()
    T = np.array([V @ t.T for t in tris]).transpose(0, 2, 1)
    allp = T.reshape(-1, 3)
    N = S * ss
    c, R = enclosing_circle(allp[:, :2])
    pad = 0.035 * N + line_px * ss
    scale = (N / 2 - pad) / max(R, 1e-6)
    def to_px(q):
        x = N / 2 + (q[..., 0] - c[0]) * scale
        y = N / 2 - (q[..., 1] - c[1]) * scale
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
        g = 1.0 if lam > 0.8 else 0.74 + 0.26 * lam
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
        k = max(2, int(np.hypot(*(b[:2] - a[:2]))))
        pts = a + (b - a) * np.linspace(0, 1, k)[:, None]
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
    skip_studs = sum(st for _, st in lines) > 4000
    for ln, st in lines:
        if skip_studs and st: continue
        draw(to_px((V @ ln.T).T))
    finite = np.isfinite(zbuf)
    far = np.where(finite, zbuf, zbuf[finite].max() + 1e6)
    thr = 1.5 * scale if not skip_studs else far.max() - far[finite].min() + 1
    Pd = np.pad(far, 1, constant_values=far.max() + 1e6)
    sil = np.zeros((N, N), bool)
    for dy in (-1, 0, 1):
        for dx in (-1, 0, 1):
            if dy or dx:
                sil |= finite & (Pd[1 + dy:1 + dy + N, 1 + dx:1 + dx + N] - far > thr)
    rad = max(1, int(round(width / 2)))
    yy, xx = np.mgrid[-rad:rad + 1, -rad:rad + 1]
    sil = ndimage.binary_dilation(sil, structure=(xx ** 2 + yy ** 2) <= rad ** 2)
    ink = np.maximum(np.asarray(ink_img, dtype=float) / 255, sil.astype(float))
    rgba = np.zeros((N, N, 4), np.uint8)
    rgba[..., :3] = (np.clip(shade * (1 - ink), 0, 1)[..., None] * 255).astype(np.uint8)
    rgba[..., 3] = (np.maximum(finite, ink) * 255).astype(np.uint8)
    Image.fromarray(rgba, "RGBA").resize((S, S), Image.LANCZOS).save(out_png)
    return True

def safe(p): return re.sub(r"[^A-Za-z0-9._-]", "_", p)

def work(args):
    part, out, size, force, local = args
    global LOCAL; LOCAL = local
    f, desc, cat = resolve(part)
    icon = os.path.join(out, safe(part) + ".png")
    if f is None:
        return dict(part_num=part, ldraw_file="", description="", ldraw_category="", icon="", status="not_found")
    if os.path.exists(icon) and not force:
        return dict(part_num=part, ldraw_file=f, description=desc, ldraw_category=cat, icon=os.path.basename(icon), status="cached")
    try:
        ok = render(f, icon, S=size)
    except Exception as e:
        ok = False; print(f"  render error {part}: {e}", file=sys.stderr)
    return dict(part_num=part, ldraw_file=f, description=desc, ldraw_category=cat,
                icon=os.path.basename(icon) if ok else "", status="ok" if ok else "no_geometry")

def read_parts(path):
    txt = open(path, encoding="utf-8").read().splitlines()
    if path.lower().endswith(".csv"):
        rows = list(csv.reader(txt))
        hdr = [h.strip().lower() for h in rows[0]]
        col = next((hdr.index(c) for c in ("part_num", "part", "part_number") if c in hdr), None)
        rows = rows[1:] if col is not None or not re.match(r"^[0-9a-z]", rows[0][0].lower()) else rows
        col = col or 0
        parts = [r[col] for r in rows if r]
    else:
        parts = [l.strip() for l in txt if l.strip()]
    out = []
    for p in parts:
        p = p.split("#")[0].strip().removesuffix(".dat")
        if p and p not in out: out.append(p)
    return out

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--parts", required=True); ap.add_argument("--out", default="icons")
    ap.add_argument("--ldraw-dir", default=None); ap.add_argument("--size", type=int, default=640)
    ap.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 2) - 1))
    ap.add_argument("--force", action="store_true")
    a = ap.parse_args()
    os.makedirs(a.out, exist_ok=True)
    marker = os.path.join(a.out, ".style")
    if not a.force and (not os.path.exists(marker) or open(marker).read().strip() != STYLE):
        if any(f.endswith(".png") for f in os.listdir(a.out)):
            print("icon style changed since last run: re-rendering all icons", flush=True)
        a.force = True
    open(marker, "w").write(STYLE)
    parts = read_parts(a.parts)
    print(f"{len(parts)} distinct parts -> {a.out}", flush=True)
    rows = []
    with cf.ProcessPoolExecutor(a.jobs) as ex:
        for r in ex.map(work, [(p, a.out, a.size, a.force, a.ldraw_dir) for p in parts]):
            print(f"  {r['part_num']:14s} {r['status']:12s} {r['description'][:50]}", flush=True)
            rows.append(r)
    # shared icon library: merge with the existing manifest (keep rows of parts not in this run)
    mf = os.path.join(a.out, "manifest.csv")
    if os.path.exists(mf):
        old = {r["part_num"]: r for r in csv.DictReader(open(mf, newline=""))}
        for r in rows:
            if r["status"] in ("ok", "cached") or r["part_num"] not in old: old[r["part_num"]] = r
        merged = list(old.values())
    else:
        merged = rows
    with open(mf, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys())); w.writeheader(); w.writerows(merged)
    bad = [r["part_num"] for r in rows if r["status"] not in ("ok", "cached")]
    print(f"done: {len(rows) - len(bad)} icons, {len(bad)} missing {bad[:20]}")

if __name__ == "__main__":
    main()
