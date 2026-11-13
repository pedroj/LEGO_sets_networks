#!/usr/bin/env python3
"""10174 (Imperial AT-ST): vectorised geometry classification of ALL instance pairs.
World AABB of every instance from the real LDraw bounding box of its part (studs included, part_bbox.csv) and its rotation;
overlaps o = (ox, oy, oz) between boxes (negative = gap). Joint family from the project's compatibility matrix
(subtype x subtype) via 10174_nodes.csv subtypes.  Instances are in build order (column i of 10174_instances.csv)."""
import csv, numpy as np
inst = list(csv.DictReader(open('10174_instances.csv')))
bb = {r['part']: {k: float(r[k]) for k in r if k != 'part'} for r in csv.DictReader(open('part_bbox.csv'))}
N = len(inst)
names = [x['node'] for x in inst]; dat = [x['dat'] for x in inst]
LO = np.zeros((N, 3)); HI = np.zeros((N, 3))
for n, x in enumerate(inst):
    R = [float(v) for v in x['R'].split()]
    M = np.array([[R[0], R[3], R[6]], [R[1], R[4], R[7]], [R[2], R[5], R[8]]])
    if x['dat'] not in bb:                      # flexible hose: no rigid box; handled from the kernel (segment-wise) in check_with_instructions.py
        LO[n] = HI[n] = 1e6 + 1000 * n; continue
    b = bb[x['dat']]
    pts = np.array([[cx, cy, cz] for cx in (b['xmin'], b['xmax']) for cy in (b['ymin'], b['ymax']) for cz in (b['zmin'], b['zmax'])])
    w = pts @ M.T + np.array([float(x['x']), float(x['y']), float(x['z'])])
    LO[n], HI[n] = w.min(0), w.max(0)
# subtype / compatibility
sub = {r['part']: r['subtype'] for r in csv.DictReader(open('10174_nodes.csv'))}
# per-set corrections of the automatic classification (same as .PART_OVERRIDE in run_plot*.R)
cm = list(csv.reader(open('../../data/lego_compat_out/compat_matrix_subtype_codes.csv')))
hdr = cm[0][1:]; CM = {r[0]: dict(zip(hdr, r[1:])) for r in cm[1:]}
PAIR_ALLOW = {}
code = lambda a, b: PAIR_ALLOW.get(frozenset((a, b))) or CM.get(sub.get(a, ''), {}).get(sub.get(b, ''), '')
grp = [x['grp'].split(' - ')[-1] for x in inst]

def overlaps(i, J):
    return np.minimum(HI[i], HI[J]) - np.maximum(LO[i], LO[J])      # (len(J), 3)
def kind(o):
    ox, oy, oz = o[:, 0], o[:, 1], o[:, 2]
    k = np.full(len(o), 'near', dtype=object)
    k[(ox > 1) & (oz > 1) & (oy > 5.5)] = 'inside'
    k[(ox > 1) & (oz > 1) & (oy >= -1) & (oy <= 5.5)] = 'stack'
    k[(ox > 1) & (oz > 1) & (oy < -1)] = 'gap'
    side = (oy > 5.5) & (((ox > 1) & (oz >= -1.5) & (oz <= 1.5)) | ((oz > 1) & (ox >= -1.5) & (ox <= 1.5)))
    k[side] = 'side'
    return k

def all_pairs(tol=6.0):
    """candidate pairs whose boxes overlap or are within tol LDU in every axis"""
    out = []
    for i in range(N - 1):
        J = np.arange(i + 1, N); o = overlaps(i, J)
        m = (o > -tol).all(1)
        if m.any():
            Jm = J[m]; om = o[m]; km = kind(om)
            for j, oo, kk in zip(Jm, om, km): out.append((i, int(j), oo, kk))
    return out

if __name__ == '__main__':
    import collections, pickle
    pr = all_pairs()
    kern = list(csv.reader(open('10174_adjacency_matrix.csv')))
    kn = [n.strip('"') for n in kern[0][1:]]; assert kn == names, 'kernel order differs from instance order'
    K = np.array([[float(v) for v in r[1:]] for r in kern[1:]]) > 0
    rows = []
    for i, j, o, k in pr:
        rows.append((i, j, k, code(dat[i], dat[j]), bool(K[i, j]), o))
    pickle.dump(rows, open('pairs.pkl', 'wb'))
    print('candidate pairs', len(rows), 'kernel links', int(K.sum() // 2), 'kernel links among candidates', sum(r[4] for r in rows))
    c = collections.Counter((r[2], r[3] != '', r[4]) for r in rows)
    for k, v in sorted(c.items()): print(k, v)
