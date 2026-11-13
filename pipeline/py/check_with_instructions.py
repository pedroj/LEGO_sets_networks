"""Shared link audit: kernel links vs real LDraw geometry (+ compat rules + hand-checked links from the instruction booklet).
Writes <set>_adjacency_matrix_checked.csv, <set>_link_audit.csv, <set>_link_audit_summary.md.
Run after check_geometry.py (needs pairs.pkl). Set-specific input comes only from set_config.yaml."""
import pickle, collections, csv, numpy as np
import check_geometry as G
import decide as D
from setcfg import C, SET, F, MANUAL_ADD, MANUAL_REMOVE, BRIDGE, FLEXIBLE

rows = pickle.load(open('pairs.pkl', 'rb'))
names, dat = G.names, G.dat
N = len(names); X = {n: k for k, n in enumerate(names)}
K = G.read_kernel(); n_k = int(K.sum() // 2)
acc = {}
for i, j, k, cd, inK, o in rows:
    ok, why = D.engaged(o, cd)
    if ok: acc[(i, j)] = (k, cd, inK, 'keep' if inK else 'add (false negative)', why)
# flexible parts (hoses) have no rigid box: their kernel links, computed segment by segment, are kept
for h in [i for i in range(N) if dat[i] in FLEXIBLE]:
    for j in np.where(K[h])[0]:
        acc[(min(h, j), max(h, j))] = ('hose', 'flex', True, 'keep (hose, kernel)', 'flexible hose: kernel contact from the segment boxes kept')
# hand-checked removals (false positives), by node name in set_config.yaml; applied before the additions
bad = [(a, b) for a, b, _ in MANUAL_REMOVE if a not in X or b not in X]
assert not bad, 'manual_remove names not found in instances: %s' % bad
REM = {tuple(sorted((X[a], X[b]))): why for a, b, why in MANUAL_REMOVE}
for key in REM: acc.pop(key, None)
# hand-checked links (geometry + building-instruction pictures), given by node name in set_config.yaml
bad = [(a, b) for a, b, _, _ in MANUAL_ADD if a not in X or b not in X]
assert not bad, 'manual_add names not found in instances: %s' % bad
for a, b, jt, why in MANUAL_ADD:
    i, j = sorted((X[a], X[b]))
    if (i, j) not in acc:
        inK = bool(K[i, j])     # a hand-checked link already in the starting matrix is a confirmation, not an addition
        acc[(i, j)] = ('manual', jt, inK, 'keep (hand-checked)' if inK else 'add (false negative)', why)
deg = np.zeros(N, int)
for (i, j) in acc: deg[i] += 1; deg[j] += 1
for i, j, k, cd, inK, o in rows:     # pass 2: isolated parts keep geometry-confirmed kernel links
    if (i, j) in acc or not inK or (i, j) in REM: continue
    o = np.asarray(o)
    if (deg[i] == 0 or deg[j] == 0) and (o >= -1.0).all() and (o >= 3).sum() >= 2:
        acc[(i, j)] = (k, cd, inK, 'keep (geometry-confirmed)',
                       'kernel link kept: isolated part touches partner in LDraw geometry; subtype matrix gives no joint')
        deg[i] += 1; deg[j] += 1
if BRIDGE:
    # pass 3: bridge sub-assemblies. A sub-model joined to the rest only by hinge/clip/ball/pin joints that the box rules reject would
    # float as a separate component. Candidate bridges are kernel links whose boxes touch in the real LDraw geometry
    # (o >= -1.5 everywhere, >= 3 LDU on two axes); those whose subtype pair has a joint in the matrix come first,
    # then the best-touching ones without a joint. Added until no further merge is possible; one round per component pair.
    par3 = list(range(N))
    def f3(x):
        while par3[x] != x: par3[x] = par3[par3[x]]; x = par3[x]
        return x
    for (i, j) in acc: par3[f3(i)] = f3(j)
    def touch(o):
        o = np.asarray(o); return (o >= -1.5).all() and (o >= 3).sum() >= 2
    for phase in ('joint', 'nojoint'):
        cand = []
        for i, j, k, cd, inK, o in rows:
            if (i, j) in acc or (i, j) in REM or not inK or not touch(o): continue
            if (cd != '') != (phase == 'joint'): continue
            cand.append((-float(np.minimum(np.asarray(o), 20).sum()), i, j, k, cd))
        cand.sort()
        seen_pairs = set()
        for _, i, j, k, cd in cand:
            a_, b_ = f3(i), f3(j)
            if a_ == b_: continue
            key = (min(a_, b_), max(a_, b_))
            if phase == 'nojoint' and key in seen_pairs: continue      # joint-less bridges: only one link per component pair
            seen_pairs.add(key)
            acc[(i, j)] = (k, cd, True, 'keep (bridge)', 'sub-assembly bridge: kernel link whose boxes touch in the real geometry' + ('' if cd else '; subtype matrix gives no joint'))
            par3[f3(i)] = f3(j)
    deg = np.zeros(N, int)
    for (i, j) in acc: deg[i] += 1; deg[j] += 1
cnt = collections.Counter()
with open(F['audit'], 'w', newline='') as fh:
    w = csv.writer(fh)
    w.writerow(['inst_a', 'inst_b', 'part_a', 'part_b', 'kind', 'joint', 'in_kernel', 'decision', 'reason'])
    for i, j, k, cd, inK, o in rows:
        if (i, j) in acc: continue
        dec = 'remove' if inK else 'reject'; cnt[dec] += 1
        w.writerow([names[i], names[j], dat[i], dat[j], k, cd, inK, dec, REM.get((i, j)) or D.engaged(o, cd)[1]])
    for (i, j), (k, cd, inK, dec, why) in acc.items():
        cnt[dec] += 1
        w.writerow([names[i], names[j], dat[i], dat[j], k, cd, inK, dec, why])
M = np.zeros((N, N), int)
for (i, j) in acc: M[i, j] = M[j, i] = 1
with open(F['checked'], 'w', newline='') as fh:
    w = csv.writer(fh); w.writerow([''] + names)
    for a in range(N): w.writerow([names[a]] + M[a].tolist())
par = list(range(N))
def f(x):
    while par[x] != x: par[x] = par[par[x]]; x = par[x]
    return x
for (i, j) in acc: par[f(i)] = f(j)
comp = collections.Counter(f(i) for i in range(N))
kept_k = sum(1 for v in acc.values() if v[2]); added = len(acc) - kept_k
txt = ('# %s link audit\n\n'
 'Starting matrix: %d instance links. Checked: %d links (%d kept from kernel, %d added = false negatives).\n\n'
 'Starting links removed as false positives: %d (of which %d are candidate pairs failing the geometry rules; the rest are pairs more than %g LDU apart).\n\n'
 'Connected components: %d (largest %d of %d parts). Isolated parts: %d.\n\n'
 'Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. %d links were added and %d removed by hand after comparing the geometry with the instruction pictures.\n'
 ) % (SET, n_k, len(acc), kept_k, added, n_k - kept_k, cnt['remove'], G.TOL if hasattr(G, 'TOL') else 6, len(comp), max(comp.values()), N, int((deg == 0).sum()), len(MANUAL_ADD), len(MANUAL_REMOVE))
if C.get('audit_notes'): txt += C['audit_notes'].strip() + '\n'
open(F['summary'], 'w').write(txt)
print(txt); print(cnt); print(sorted(comp.values(), reverse=True)[:12])
