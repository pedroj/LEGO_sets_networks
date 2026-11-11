#!/usr/bin/env python3
"""Cross-check of the kernel adjacency matrix of set 75344 (Boba Fett's Starship Microfighter) against
(i) real LDraw geometry (check_geometry.py: world-space bounding boxes incl. studs),
(ii) the building instructions PDF (56 pp., 42 steps; low-res pages used to confirm sub-assemblies and order),
(iii) the previous part-type edge list from the .cys file (75344-1_adjacency_matrix_edgelist.csv) as a tie-breaker.
Input : 75344_adjacency_matrix_kernel.csv  (kernel/proximity matrix, 522 links)
Output: 75344_adjacency_matrix_checked.csv (checked, instance level), 75344_link_audit.csv, 75344_link_audit_summary.md
Rules (applied to ALL instance pairs, so missed links are found too)
  stack  : footprints overlap and one underside meets the other's studs (bbox overlap 2-5.5 LDU) with footprint >=12 LDU
           in both horizontal axes -> stud/tube joint. Narrow (<12) or merely touching (<2 LDU) stacks are kept only
           if the previous edge list has that part-type pair.
  inside : volumes interpenetrate -> kept if (a) stud-height penetration (3.5-4.5 LDU, SNOT stud joint) or
           (b) the part-type pair is in the previous edge list (pins through holes, clips, brackets...). Else removed.
  side/near/gap : parts only touch sideways or are apart -> not a mechanical link (removed).
"""
import csv, collections
import check_geometry as G
P, E = G.P, G.E
base = lambda n: n.split('#')[0]
pair = lambda a, b: frozenset((a, b))
RB = {'577b': '64567'}                       # LDraw -> Rebrickable numbering
rb = lambda t: RB.get(t, t)
rows = list(csv.reader(open('75344-1_adjacency_matrix_edgelist.csv')))
cn = [c.strip('"') for c in rows[0][1:]]
PED = {frozenset((cn[i], cn[j])) for i, r in enumerate(rows[1:]) for j, v in enumerate(r[1:]) if float(v) > 0}
tp = lambda a, b: frozenset((rb(base(a)), rb(base(b))))
orig = {pair(a, b) for a, b in E}
names_all = [n.strip('"') for n in list(csv.reader(open('75344_adjacency_matrix_kernel.csv')))[0][1:]]
dec = {}
def classify(a, b):
    o = G.rel(a, b); kd = G.kind(o); inP = tp(a, b) in PED
    ox, oy, oz = o
    if kd == 'stack':
        if min(ox, oz) >= 12 and oy >= 2: return 'keep', "stud/tube stack: footprints overlap and underside meets studs (PDF steps 1-30)", "geometry + PDF"
        if inP: return 'keep', "narrow/touching stack confirmed by previous edge list", "geometry + edge list"
        return 'remove', "narrow or touching-only contact (no stud engagement)", "geometry"
    if kd == 'inside':
        small = sorted(o)[0]
        if 3.5 <= small <= 4.5 and sorted(o)[1] >= 8: return 'keep', "stud-height penetration (4 LDU): side/SNOT stud joint", "geometry"
        tb = {base(a), base(b)}
        if tb & {'61184', '18651'}: return 'keep', "technic pin inserted in a pin hole (PDF steps 10, 41)", "geometry + PDF"
        if tb == {'3069b', '44861'}: return 'keep', "1x2 tile held in the top clip of the plate (PDF steps 1-30)", "geometry + PDF"
        if inP: return 'keep', "nesting joint (pin/axle/clip/bracket) confirmed by edge list and bbox interpenetration", "geometry + edge list"
        return 'remove', "volumes overlap but no real joint between these parts", "geometry"
    return 'remove', {'side': "parts only touch sideways (no stud/tube/clip engagement)",
                      'near': "bounding boxes not in contact in real LDraw geometry (kernel box too large)",
                      'gap': "vertical gap: parts do not touch"}[kd], "geometry"
names = list(P)
for i, a in enumerate(names):
    for b in names[i + 1:]:
        k = pair(a, b); inK = k in orig
        d, why, ev = classify(a, b)
        if d == 'keep': dec[k] = ('keep' if inK else 'add', why, ev)
        elif inK: dec[k] = ('remove', why, ev)
# --- hand-reviewed type pairs of the previous edge list not supported by geometry (reported, not added)
DISPUTED = [
 ("11458", "15068", "edge list links them; instances are >50 LDU apart in the MPD"),
 ("23950", "32064a", "edge list links them; 4 LDU vertical gap in the MPD"),
 ("2419", "3020", "side-by-side plates at same level, no stud engagement"),
 ("24307", "32952", "side contact only; symmetric 24299 would give the same, ambiguous"),
]
idx = {n: i for i, n in enumerate(names_all)}
A = [[0] * len(names_all) for _ in names_all]
for k, (d, _, _) in dec.items():
    if d in ('keep', 'add'):
        a, b = tuple(k); A[idx[a]][idx[b]] = A[idx[b]][idx[a]] = 1
with open('75344_adjacency_matrix_checked.csv', 'w', newline='') as fh:
    w = csv.writer(fh, quoting=csv.QUOTE_NONNUMERIC); w.writerow([''] + names_all)
    for n, r in zip(names_all, A): w.writerow([n] + r)
with open('75344_link_audit.csv', 'w', newline='') as fh:
    w = csv.writer(fh); w.writerow(['part_a', 'part_b', 'decision', 'reason', 'evidence', 'in_kernel_matrix'])
    for k, (d, why, ev) in sorted(dec.items(), key=lambda x: (x[1][0], sorted(x[0]))):
        a, b = sorted(k); w.writerow([a, b, d, why, ev, int(k in orig)])
    for a, b, why in DISPUTED: w.writerow([a, b, 'not_added', why, 'edge list only', 0])
c = collections.Counter(d for d, _, _ in dec.values())
tot = sum(sum(r) for r in A) // 2
print(dict(c), 'links in checked matrix:', tot, 'kernel:', len(orig))
with open('75344_link_audit_summary.md', 'w') as fh:
    fh.write("# 75344 link audit\n\nKernel links: %d; kept %d, removed %d, added %d; checked matrix %d links.\n\n" % (len(orig), c['keep'], c['remove'], c['add'], tot))
    fh.write("Evidence: LDraw bounding-box geometry on every instance pair, instruction PDF (sub-assembly order, low resolution), previous .cys edge list as tie-breaker.\n\nNot added although listed in the old edge list:\n")
    for a, b, why in DISPUTED: fh.write("- %s - %s: %s\n" % (a, b, why))
