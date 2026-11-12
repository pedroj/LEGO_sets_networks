"""4488 link audit: kernel links vs real LDraw geometry (+ compat rules + build order from the MPD).
Writes 4488_adjacency_matrix_checked.csv, 4488_link_audit.csv, 4488_link_audit_summary.md
Run after check_geometry.py (needs pairs.pkl). No pandas needed."""
import pickle, collections, csv, numpy as np
import check_geometry as G
import decide as D

rows = pickle.load(open('pairs.pkl', 'rb'))
names, dat = G.names, G.dat
N = len(names)
n_k = int(sum(float(v) for r in list(csv.reader(open('4488_adjacency_matrix_kernel.csv')))[1:] for v in r[1:]) // 2)   # kernel instance-level links
acc = {}
for i, j, k, cd, inK, o in rows:
    ok, why = D.engaged(o, cd)
    if ok: acc[(i, j)] = (k, cd, inK, 'keep' if inK else 'add (false negative)', why)
deg = np.zeros(N, int)
for (i, j) in acc: deg[i] += 1; deg[j] += 1

# ---- hand-checked links (geometry + building-instruction pictures, steps 1-8 and roof sub-assembly) ---------------
# (instance index i, j, joint, reason); indices = row order of 4488_instances.csv
X = {n: k for k, n in enumerate(names)}
MANUAL_ADD = [
    ('2436a#1', '3710#1', 'stud', 'bracket 2436a plugs onto the studs of plate 3710#1 (step 7, picture of the sub-assembly 1); its tall flange makes the boxes overlap more than a stud height'),
    ('3941#1', '3961ps0#1', 'stud', 'upper dish drops onto the stud of the blue round brick 3941#1 (step 17 and the picture on page 17)'),
    ('3705#1', '3961ps0#1', 'axle', 'tip of the axle 3705 enters the upper dish (step 17 picture); the subtype matrix gives no dish-axle joint'),
    ('3961#1', '4032a#2', 'stud', 'blue round plate 4032a#2 sits on the lower dish 3961 (step 16 picture); dish studs inside the plate make the overlap too large for the stud rule'),
    ('3023#1', '2444#2', 'stud', 'plate 2444#2 sits on plate 3023#1 (step 10 callout and picture); the underside boss of 2444 inflates its box so the stud rule fails'),
    ('3023#2', '2444#1', 'stud', 'plate 2444#1 sits on plate 3023#2 (step 10 callout and picture); the underside boss of 2444 inflates its box so the stud rule fails'),
]
MANUAL_ADD = [(X[a], X[b], jt, why) for a, b, jt, why in MANUAL_ADD]
MANUAL_REMOVE = [
    ('3961ps0#1', '4593#1', 'lever 4593#1 only rests on the dish top (face contact); it is held by its base 4592#1'),
    ('3961ps0#1', '4593#2', 'lever 4593#2 only rests on the dish top (face contact); it is held by its base 4592#2'),
    ('32001#1', '43093#1', 'axle pin 43093#1 lies above the end of the 2x6 plate and only touches its edge; it is held by plate 2444#1 and round brick 3941#3 (step 10 callout)'),
    ('32001#1', '43093#2', 'axle pin 43093#2 lies above the end of the 2x6 plate and only touches its edge; it is held by plate 2444#2 and round brick 3941#2 (step 10 callout)'),
    ('4740#1', '30565#10', 'dish 4740 is held only by the tap 4599a (callout of the last step); it does not engage the plate under it'),
]
MANUAL_REMOVE = [(X[a], X[b], why) for a, b, why in MANUAL_REMOVE]
REM = {tuple(sorted((i, j))): why for i, j, why in MANUAL_REMOVE}
for key in REM: acc.pop(key, None)
for i, j, jt, why in MANUAL_ADD:
    if (i, j) not in acc: acc[(i, j)] = ('manual', jt, False, 'add (false negative)', why)
deg = np.zeros(N, int)
for (i, j) in acc: deg[i] += 1; deg[j] += 1
for i, j, k, cd, inK, o in rows:     # pass 2: isolated parts keep geometry-confirmed kernel links
    if (i, j) in acc or not inK or (i, j) in REM: continue
    o = np.asarray(o)
    if (deg[i] == 0 or deg[j] == 0) and (o >= -1.0).all() and (o >= 3).sum() >= 2:
        acc[(i, j)] = (k, cd, inK, 'keep (geometry-confirmed)',
                       'kernel link kept: isolated part touches partner in LDraw geometry; subtype matrix gives no joint')
        deg[i] += 1; deg[j] += 1
cnt = collections.Counter()
with open('4488_link_audit.csv', 'w', newline='') as fh:
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
with open('4488_adjacency_matrix_checked.csv', 'w', newline='') as fh:
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
txt = ('# 4488 link audit (LEGO 4488-1 Millennium Falcon, mini)\n\n'
 'Kernel: %d instance links. Checked: %d links (%d kept from kernel, %d added = false negatives).\n\n'
 'Kernel links removed as false positives: %d (of which %d are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).\n\n'
 'Connected components: %d (largest %d of %d parts). Isolated parts: %d.\n\n'
 'Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush face or side contact is not a joint); non-stud joints (pin, axle, hinge, bar...) when the boxes touch and the subtype matrix allows the joint.\n\n'
 'Instruction check: the 19 STEP blocks of the MPD match the 19 Falcon steps of the booklet (pages 3-19); the rest of the booklet covers the other three mini sets (4489-4491). '
 '%d links were added by hand after comparing geometry with the pictures (bracket 2436a on plate 3710#1, upper dish on round brick 3941#1 and on the axle tip, blue plate 4032a#2 on the lower dish, two plates 2444 on plates 3023) and %d kernel/rule links were removed by hand (levers resting on the dish, axle pins lying above the 2x6 plate end, radar dish 4740 that only hangs on the tap).\n\n'
 'Parts reclassified for the check: tap 4599a as a 1x1 brick, plate round with axle hole 4032a as a technic plate (it takes the axle 3705). Extra joints allowed: lever 4593 on base 4592 (hinge), tap 4599a - dish 4740 (spout takes the pin), dome 30367a - pin 4274, round tile with bottom cross 4150 - axle 3705, plate 2444 - axle pin 43093 (pin).\n'
 ) % (n_k, len(acc), kept_k, added, n_k - kept_k, cnt['remove'], len(comp), max(comp.values()), N, int((deg == 0).sum()), len(MANUAL_ADD), len(MANUAL_REMOVE))
open('4488_link_audit_summary.md', 'w').write(txt)
print(txt); print(cnt); print(sorted(comp.values(), reverse=True)[:12])
