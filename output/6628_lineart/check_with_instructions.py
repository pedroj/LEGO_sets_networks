"""6628 link audit: kernel links vs real LDraw geometry (+ compat rules + build order from the MPD).
Writes 6628_adjacency_matrix_checked.csv, 6628_link_audit.csv, 6628_link_audit_summary.md
Run after check_geometry.py (needs pairs.pkl). No pandas needed."""
import pickle, collections, csv, numpy as np
import check_geometry as G
import decide as D

rows = pickle.load(open('pairs.pkl', 'rb'))
names, dat = G.names, G.dat
N = len(names)
n_k = int(sum(float(v) for r in list(csv.reader(open('6628_adjacency_matrix_kernel.csv')))[1:] for v in r[1:]) // 2)   # kernel instance-level links
acc = {}
for i, j, k, cd, inK, o in rows:
    ok, why = D.engaged(o, cd)
    if ok: acc[(i, j)] = (k, cd, inK, 'keep' if inK else 'add (false negative)', why)
deg = np.zeros(N, int)
for (i, j) in acc: deg[i] += 1; deg[j] += 1

# ---- hand-checked links (geometry + building-instruction pictures, steps 1-8 and roof sub-assembly) ---------------
# (instance index i, j, joint, reason); indices = row order of 6628_instances.csv
MANUAL_ADD = [
    (14, 17, 'stud', 'ladder plate 4175 sits on plate 3023#2 (stack of 3710#3 > 3023#3 > 3023#2 > 4175; box of 4175 includes its ladder)'),
    (21, 38, 'stud', 'plate 3710#2 sits in the hollow underside of the crane slope 3135 (steps 5-6)'),
    (3, 36, 'stud', 'steering-wheel stand 3829a plugged into the opening of the vehicle base 4211 (step 5)'),
    (36, 37, 'stud', 'steering wheel 3828 on its stand 3829a (step 5)'),
    (5, 19, 'door', 'right door 3822p60 hinges on the plate 3623 below it (step 7)'),
    (6, 18, 'door', 'left door 3821p60 hinges on the plate 3623 below it (step 7)'),
    (5, 33, 'door', 'door 3822p60 held by the windscreen 3823 above it (step 8)'),
    (6, 33, 'door', 'door 3821p60 held by the windscreen 3823 above it (step 8)'),
    (38, 39, 'stud', 'tow hook 3136 hangs on the tip of the crane 3135 (step 5; kernel left it isolated)'),
]
for i, j, jt, why in MANUAL_ADD:
    if (i, j) not in acc: acc[(i, j)] = ('manual', jt, False, 'add (false negative)', why)
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
with open('6628_link_audit.csv', 'w', newline='') as fh:
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
with open('6628_adjacency_matrix_checked.csv', 'w', newline='') as fh:
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
txt = ('# 6628 link audit\n\n'
 'Kernel: %d instance links. Checked: %d links (%d kept from kernel, %d added = false negatives).\n\n'
 'Kernel links removed as false positives: %d (of which %d are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).\n\n'
 'Connected components: %d (largest %d of %d parts). Isolated parts: %d.\n\n'
  'Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 9 links were added by hand after comparing the geometry with the instruction pictures (steps 1-8 and the roof sub-assembly): doors, steering wheel, tow hook, ladder plate, plate inside the crane slope.\n'
 'The MPD lists parts by position (no STEP lines), so it gives no build order; the instruction pictures were used instead. 3 vehicle parts were reclassified (4211 base and 3788 mudguard as bricks, 3136 hook as plate with studs); the tyre-on-wheel joint 122c01-4084 was allowed.\n'
 ) % (n_k, len(acc), kept_k, added, n_k - kept_k, cnt['remove'], len(comp), max(comp.values()), N, int((deg == 0).sum()))
open('6628_link_audit_summary.md', 'w').write(txt)
print(txt); print(cnt); print(sorted(comp.values(), reverse=True)[:12])
