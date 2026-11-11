"""75181 link audit: kernel links vs real LDraw geometry (+ compat rules + build order from the MPD).
Writes 75181_adjacency_matrix_checked.csv, 75181_link_audit.csv, 75181_link_audit_summary.md
Run after check_geometry.py (needs pairs.pkl). No pandas needed."""
import pickle, collections, csv, numpy as np
import check_geometry as G
import decide as D

rows = pickle.load(open('pairs.pkl', 'rb'))
names, dat = G.names, G.dat
N = len(names)
n_k = 15816                       # kernel instance-level links
acc = {}
for i, j, k, cd, inK, o in rows:
    ok, why = D.engaged(o, cd)
    if ok: acc[(i, j)] = (k, cd, inK, 'keep' if inK else 'add (false negative)', why)
deg = np.zeros(N, int)
for (i, j) in acc: deg[i] += 1; deg[j] += 1
for i, j, k, cd, inK, o in rows:     # pass 2: isolated parts keep geometry-confirmed kernel links
    if (i, j) in acc or not inK: continue
    o = np.asarray(o)
    if (deg[i] == 0 or deg[j] == 0) and (o >= -1.0).all() and (o >= 3).sum() >= 2:
        acc[(i, j)] = (k, cd, inK, 'keep (geometry-confirmed)',
                       'kernel link kept: isolated part touches partner in LDraw geometry; subtype matrix gives no joint')
        deg[i] += 1; deg[j] += 1
cnt = collections.Counter()
with open('75181_link_audit.csv', 'w', newline='') as fh:
    w = csv.writer(fh)
    w.writerow(['inst_a', 'inst_b', 'part_a', 'part_b', 'kind', 'joint', 'in_kernel', 'decision', 'reason'])
    for i, j, k, cd, inK, o in rows:
        if (i, j) in acc: continue
        dec = 'remove' if inK else 'reject'; cnt[dec] += 1
        w.writerow([names[i], names[j], dat[i], dat[j], k, cd, inK, dec, D.engaged(o, cd)[1]])
    for (i, j), (k, cd, inK, dec, why) in acc.items():
        cnt[dec] += 1
        w.writerow([names[i], names[j], dat[i], dat[j], k, cd, inK, dec, why])
M = np.zeros((N, N), int)
for (i, j) in acc: M[i, j] = M[j, i] = 1
with open('75181_adjacency_matrix_checked.csv', 'w', newline='') as fh:
    w = csv.writer(fh); w.writerow([''] + names)
    for a in range(N): w.writerow([names[a]] + M[a].tolist())
par = list(range(N))
def f(x):
    while par[x] != x: par[x] = par[par[x]]; x = par[x]
    return x
for (i, j) in acc: par[f(i)] = f(j)
comp = collections.Counter(f(i) for i in range(N))
earlier = np.zeros(N, int)
for (i, j) in acc: earlier[max(i, j)] += 1
noprev = [names[i] for i in range(N) if earlier[i] == 0]
kept_k = sum(1 for v in acc.values() if v[2]); added = len(acc) - kept_k
txt = ('# 75181 link audit\n\n'
 'Kernel: %d instance links. Checked: %d links (%d kept from kernel, %d added = false negatives).\n\n'
 'Kernel links removed as false positives: %d (of which %d are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).\n\n'
 'Connected components: %d (largest %d of %d parts). Parts with no link to an earlier-built part: %d.\n\n'
 'Rules: stud engagement (box contact along any axis within -1..5.5 LDU with >=8 LDU footprint, SNOT included); non-stud joints (pin, axle, bar, clip, ball, hinge...) when boxes touch/interpenetrate and the subtype matrix allows the joint; isolated parts keep kernel links that touch in LDraw geometry.\n'
 'Build order = instance expansion order in the MPD; the instruction PDF was used for sub-assembly sampling only.\n'
 ) % (n_k, len(acc), kept_k, added, n_k - kept_k, cnt['remove'], len(comp), max(comp.values()), N, len(noprev))
open('75181_link_audit_summary.md', 'w').write(txt)
print(txt); print(cnt); print(sorted(comp.values(), reverse=True)[:12])
