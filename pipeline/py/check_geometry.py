#!/usr/bin/env python3
"""Shared: vectorised geometry classification of ALL instance pairs of one set.
World AABB of every instance from the real LDraw bounding box of its part (studs included, part_bbox.csv) and its rotation;
overlaps o = (ox, oy, oz) between boxes (negative = gap). Joint family from the compatibility matrix (subtype x subtype).
Run from output/<set>_lineart/:  python3 ../../pipeline/py/check_geometry.py   -> pairs.pkl"""
import csv, numpy as np
from setcfg import F, SUBTYPE_OVERRIDE, PAIR_ALLOW, FLEXIBLE, TOL
inst = list(csv.DictReader(open(F['instances'])))
bb = {r['part']: {k: float(r[k]) for k in r if k != 'part'} for r in csv.DictReader(open(F['bbox']))}
N = len(inst)
names = [x['node'] for x in inst]; dat = [x['dat'] for x in inst]
missing = sorted({d for d in dat if d not in bb} - FLEXIBLE)
assert not missing, 'parts without a bounding box (add to part_bbox.csv or flexible_parts): %s' % missing
LO = np.zeros((N, 3)); HI = np.zeros((N, 3))
for n, x in enumerate(inst):
    if x['dat'] in FLEXIBLE and x['dat'] not in bb:   # flexible hose: no rigid box; its kernel links are handled in check_with_instructions.py
        LO[n] = HI[n] = 1e6 + 1000 * n; continue
    R = [float(v) for v in x['R'].split()]
    M = np.array([[R[0], R[3], R[6]], [R[1], R[4], R[7]], [R[2], R[5], R[8]]])
    b = bb[x['dat']]
    pts = np.array([[cx, cy, cz] for cx in (b['xmin'], b['xmax']) for cy in (b['ymin'], b['ymax']) for cz in (b['zmin'], b['zmax'])])
    w = pts @ M.T + np.array([float(x['x']), float(x['y']), float(x['z'])])
    LO[n], HI[n] = w.min(0), w.max(0)
sub = {r['part']: r['subtype'] for r in csv.DictReader(open(F['nodes']))}
sub.update(SUBTYPE_OVERRIDE)
cm = list(csv.reader(open(F['compat'])))
hdr = cm[0][1:]; CM = {r[0]: dict(zip(hdr, r[1:])) for r in cm[1:]}
code = lambda a, b: PAIR_ALLOW.get(frozenset((a, b))) or CM.get(sub.get(a, ''), {}).get(sub.get(b, ''), '')
grp = [x['grp'].split(' - ')[-1] for x in inst]

def overlaps(i, J):
    return np.minimum(HI[i], HI[J]) - np.maximum(LO[i], LO[J])
def kind(o):
    ox, oy, oz = o[:, 0], o[:, 1], o[:, 2]
    k = np.full(len(o), 'near', dtype=object)
    k[(ox > 1) & (oz > 1) & (oy > 5.5)] = 'inside'
    k[(ox > 1) & (oz > 1) & (oy >= -1) & (oy <= 5.5)] = 'stack'
    k[(ox > 1) & (oz > 1) & (oy < -1)] = 'gap'
    side = (oy > 5.5) & (((ox > 1) & (oz >= -1.5) & (oz <= 1.5)) | ((oz > 1) & (ox >= -1.5) & (ox <= 1.5)))
    k[side] = 'side'
    return k
def all_pairs(tol=TOL):
    out = []
    for i in range(N - 1):
        J = np.arange(i + 1, N); o = overlaps(i, J)
        m = (o > -tol).all(1)
        if m.any():
            for j, oo, kk in zip(J[m], o[m], kind(o[m])): out.append((i, int(j), oo, kk))
    return out
def read_kernel():
    kern = list(csv.reader(open(F['kernel'])))
    kn = [n.strip('"') for n in kern[0][1:]]; assert kn == names, 'kernel order differs from instance order'
    return np.array([[float(v) for v in r[1:]] for r in kern[1:]]) > 0

if __name__ == '__main__':
    import collections, pickle
    K = read_kernel()
    rows = [(i, j, k, code(dat[i], dat[j]), bool(K[i, j]), o) for i, j, o, k in all_pairs()]
    pickle.dump(rows, open('pairs.pkl', 'wb'))
    print('candidate pairs', len(rows), 'kernel links', int(K.sum() // 2), 'kernel links among candidates', sum(r[4] for r in rows))
    for k, v in sorted(collections.Counter((r[2], r[3] != '', r[4]) for r in rows).items()): print(k, v)
