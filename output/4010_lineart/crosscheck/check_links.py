#!/usr/bin/env python3
"""Cross-check the 4010 adjacency matrix against real LDraw geometry.

For every part instance (same order / '#k' numbering as the matrix) it finds the
male studs (stud primitives) and tests whether each stud is inserted in another
part: the stud's middle point lies inside that part's bounding box (in the part's
own frame) while the stud's base lies outside it. Each matrix link is then
labelled by the connection found:
  stud      a stud of one part sits in the other part
  joint     known non-stud joint (minifig joints, hinge, accessory in hand ...)
  contact   the boxes touch but no stud/joint found  -> candidate false positive
  apart     the boxes do not even touch               -> likely false positive
Stud couplings that the matrix lacks are listed as candidate missing links.

usage: check_links.py MPD MATRIX_CSV LDRAW_DIR OUT_DIR
"""
import csv, os, re, sys
from functools import lru_cache
import numpy as np

MPD, MATRIX, LDRAW, OUT = sys.argv[1:5]
os.makedirs(OUT, exist_ok=True)

# ---------------------------------------------------------------- LDraw library
INDEX = {}
for sub in ("parts", "parts/s", "p", "p/48"):
    d = os.path.join(LDRAW, sub)
    if os.path.isdir(d):
        for f in os.listdir(d):
            key = (("s\\" if sub == "parts/s" else "48\\" if sub == "p/48" else "") + f).lower()
            INDEX.setdefault(key, os.path.join(d, f))

def find(name):
    n = name.lower().replace("/", "\\")
    return INDEX.get(n) or INDEX.get(os.path.basename(n))

MALE = re.compile(r"^(stud|studa|stud2|stud2a|stud6|stud6a|stud10|stud13|stud15|studel|studline|stud2s)\.dat$")
STUD_GROUP = re.compile(r"^stug")      # stud groups expand to stud refs (recursion handles them)

def mat(v):
    x, y, z, a, b, c, d, e, f, g, h, i = map(float, v)
    return np.array([[a, b, c], [d, e, f], [g, h, i]]), np.array([x, y, z])

@lru_cache(maxsize=None)
def geom(name):
    """-> (vertices Nx3 in part frame, studs list of (base, up) in part frame)"""
    path = find(name)
    if not path:
        return np.zeros((0, 3)), ()
    verts, studs = [], []
    for line in open(path, encoding="utf-8", errors="ignore"):
        t = line.split()
        if not t:
            continue
        if t[0] in ("3", "4"):
            n = int(t[0])
            verts.extend(np.array(t[2:2 + 3 * n], float).reshape(n, 3))
        elif t[0] == "1" and len(t) >= 15:
            R, p = mat(t[2:14])
            sub = " ".join(t[14:])
            base = os.path.basename(sub.lower().replace("\\", "/"))
            if MALE.match(base):
                studs.append((p, R @ np.array([0, -1.0, 0])))
                # stud geometry itself still counts for the bounding box
                verts.extend([p, p + R @ np.array([0, -4.0, 0])])
                continue
            sv, ss = geom(sub)
            if len(sv):
                verts.extend((R @ sv.T).T + p)
            for b, u in ss:
                studs.append((R @ b + p, R @ u))
    v = np.array(verts) if verts else np.zeros((0, 3))
    return v, tuple(studs)

# ---------------------------------------------------------------- MPD expansion
blocks, cur = {}, None
for line in open(MPD, encoding="utf-8", errors="ignore"):
    if line.startswith("0 FILE"):
        cur = line[7:].strip().lower()
        blocks.setdefault(cur, [])          # first definition wins
        if blocks[cur]:
            cur = None                       # duplicate FILE name: ignore later one
        continue
    if cur is not None:
        blocks[cur].append(line)

HULL_SUB = "4010 - 2372c01.dat"
parts = []                                   # (dat, R, p)
def expand(name, R0, p0):
    for line in blocks[name]:
        t = line.split()
        if len(t) < 15 or t[0] != "1":
            continue
        R, p = mat(t[2:14])
        sub = " ".join(t[14:]).lower()
        Rw, pw = R0 @ R, R0 @ p + p0
        if sub == HULL_SUB:
            sub = "2372c01.dat"
        if sub in blocks:
            expand(sub, Rw, pw)
        else:
            parts.append((sub.replace(".dat", ""), Rw, pw))

expand(next(iter(blocks)), np.eye(3), np.zeros(3))

# instance ids like the matrix: part#k in order of appearance
count, ids = {}, []
for dat, _, _ in parts:
    count[dat] = count.get(dat, 0) + 1
    ids.append(f"{dat}#{count[dat]}")

def desc(dat):
    p = find(dat + ".dat")
    return open(p, encoding="utf-8", errors="ignore").readline()[2:].strip() if p else "?"

# ---------------------------------------------------------------- per-part geometry
info = []
for (dat, R, p), iid in zip(parts, ids):
    v, studs = geom(dat + ".dat")
    lo, hi = (v.min(0), v.max(0)) if len(v) else (np.zeros(3), np.zeros(3))
    wstuds = [(R @ b + p, R @ u) for b, u in studs]
    info.append(dict(id=iid, dat=dat, R=R, p=p, lo=lo, hi=hi, studs=wstuds, desc=desc(dat)))

def inside(P, q, pad=0.5):
    """is world point q inside part P's box (in P's own frame)?"""
    l = P["R"].T @ (q - P["p"])
    return np.all(l >= P["lo"] - pad) and np.all(l <= P["hi"] + pad)

def wbox(P):
    c = np.array([[x, y, z] for x in (P["lo"][0], P["hi"][0]) for y in (P["lo"][1], P["hi"][1])
                  for z in (P["lo"][2], P["hi"][2])])
    w = (P["R"] @ c.T).T + P["p"]
    return w.min(0), w.max(0)

def contact_kind(A, B):
    """how two touching boxes meet: side by side, stacked, or intersecting"""
    a0, a1 = wbox(A); b0, b1 = wbox(B)
    o = np.minimum(a1, b1) - np.maximum(a0, b0)          # overlap per axis (x, y, z)
    if np.all(o > 2.0):
        return "boxes intersect"
    k = int(np.argmin(o))
    return "stacked, no stud" if k == 1 else "side by side"

def boxes_touch(A, B, tol=1.0):
    """world axis-aligned boxes of A and B overlap or touch within tol"""
    def wbox(P):
        c = np.array([[x, y, z] for x in (P["lo"][0], P["hi"][0]) for y in (P["lo"][1], P["hi"][1])
                      for z in (P["lo"][2], P["hi"][2])])
        w = (P["R"] @ c.T).T + P["p"]
        return w.min(0), w.max(0)
    a0, a1 = wbox(A); b0, b1 = wbox(B)
    gap = np.maximum(0, np.maximum(a0 - b1, b0 - a1))
    return float(np.linalg.norm(gap)), bool(np.all(gap <= tol))

HULL = "2372c01"
def stud_links(A, B):
    """number of A's studs inserted into B"""
    if B["dat"] == HULL:                     # the hull has no anti-studs on its upper side
        return 0
    n = 0
    for s, u in A["studs"]:
        mid, below = s + 2.0 * u, s - 1.0 * u
        if inside(B, mid, pad=0.6) and not inside(B, below, pad=-0.6):
            n += 1
    return n

# known non-stud joints (by part type pairs)
MINIFIG = {"3815", "3816", "3817", "973p1f", "973p1g", "3818", "3819", "3820", "3626bp01", "3624", "3962a"}
JOINT_PAIRS = [({"3815"}, {"3816", "3817"}), ({"3815"}, {"973p1f", "973p1g"}),
               ({"973p1f", "973p1g"}, {"3818", "3819"}), ({"3818", "3819"}, {"3820"}),
               ({"973p1f", "973p1g"}, {"3626bp01"}), ({"3626bp01"}, {"3624"}),
               ({"3820"}, {"3962a"}), ({"2348a"}, {"2349a"}), ({"298c04"}, {"4289"}),
               ({"973p1f", "973p1g"}, {"3838"}),
               # non-stud joints seen in the building instructions
               ({"4862"}, {"4863"}),          # glass clipped into window frame (step 7 inset)
               ({"4349"}, {"4873"}),          # searchlight handle in railing open stud (step 11)
               ({"4599a"}, {"4873"}),         # tap in railing open stud (step 6 inset)
               ({"4599a"}, {"4740"}),         # dish on tap (step 11 inset)
               ({"2349a"}, {"4625"}),         # hinge tile on sunroof hinge (roof sub-steps 1-2)
               ({"3838"}, {"4623"})]          # airtanks on plate with vertical bar (step 5)
def joint(a, b):
    return any((a in x and b in y) or (b in x and a in y) for x, y in JOINT_PAIRS)

# ---------------------------------------------------------------- matrix
rows = list(csv.reader(open(MATRIX, encoding="utf-8")))
hdr = rows[0][1:]
A = {r[0]: dict(zip(hdr, map(int, r[1:]))) for r in rows[1:]}
byid = {P["id"]: P for P in info}
missing_in_mpd = [h for h in hdr if h not in byid]
not_in_matrix = [P["id"] for P in info if P["id"] not in A]

S = {}
for i, P in enumerate(info):
    for Q in info[i + 1:]:
        n = stud_links(P, Q) + stud_links(Q, P)
        if n:
            S[(P["id"], Q["id"])] = S[(Q["id"], P["id"])] = n

out = []
for i, a in enumerate(hdr):
    for b in hdr[i + 1:]:
        if not A[a][b]:
            continue
        if a not in byid or b not in byid:
            out.append([a, b, "", "", "joint" if joint(a.split("#")[0], b.split("#")[0]) else "not in MPD", 0, ""]); continue
        P, Q = byid[a], byid[b]
        gap, touch = boxes_touch(P, Q)
        n = S.get((a, b), 0)
        if n:
            verdict = "stud"
        elif joint(P["dat"], Q["dat"]):
            verdict = "joint"
        elif HULL in (P["dat"], Q["dat"]):
            verdict = "hull rule"
        elif touch:
            verdict = "contact: " + contact_kind(P, Q)
        else:
            verdict = "apart"
        out.append([a, b, P["desc"], Q["desc"], verdict, n, round(gap, 1)])

with open(os.path.join(OUT, "4010_link_check.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["part_a", "part_b", "desc_a", "desc_b", "verdict", "studs_coupled", "box_gap_ldu"])
    w.writerows(out)

miss = []
for (a, b), n in S.items():
    if a < b and a in A and b in A and not A[a][b]:
        miss.append([a, b, byid[a]["desc"], byid[b]["desc"], n])
with open(os.path.join(OUT, "4010_missing_links.csv"), "w", newline="") as f:
    w = csv.writer(f); w.writerow(["part_a", "part_b", "desc_a", "desc_b", "studs_coupled"]); w.writerows(miss)

with open(os.path.join(OUT, "4010_parts_geometry.csv"), "w", newline="") as f:
    w = csv.writer(f); w.writerow(["id", "desc", "x", "y", "z", "n_studs"])
    for P in info:
        w.writerow([P["id"], P["desc"], *np.round(P["p"], 1), len(P["studs"])])

from collections import Counter
print("parts in MPD:", len(info), "| matrix nodes:", len(hdr))
print("matrix ids not in MPD:", missing_in_mpd)
print("MPD ids not in matrix:", not_in_matrix)
print("links:", len(out), Counter(r[4] for r in out))
print("stud couplings missing from matrix:", len(miss))
