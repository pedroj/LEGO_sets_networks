#!/usr/bin/env python3
"""Cross-check of the kernel adjacency matrix of set 4485 (Sebulba's + Anakin's Podracer) against
(i) real LDraw geometry (check_geometry.py: world-space boxes, 45-degree rotated parts included) and
(ii) the building instructions PDF (pp. 3-5 Anakin's Podracer steps 1-4 + pod sub-assembly; pp. 6-12 Sebulba's Podracer
    steps 1-8 + pod and plate sub-assemblies).
The model is small (24 + 43 parts) and every sub-assembly is shown, so every link is decided by hand from the build
sequence; geometry (stud-height penetration / stack / inside) is used to confirm each joint and to assign instances.
Input : 4485_adjacency_matrix_kernel.csv  (kernel/proximity matrix)
Output: 4485_adjacency_matrix_checked.csv (instance level), 4485_link_audit.csv, 4485_link_audit_summary.md
"""
import csv, collections
import check_geometry as G
P, E = G.P, G.E
pair = lambda a, b: frozenset((a, b))
base = lambda n: n.split('#')[0]
orig = {pair(a, b) for a, b in E}
JOINT = {}   # frozenset -> reason
def J(a, b, why): JOINT[pair(a, b)] = why

# ------------------------------------------------------------------ Anakin's Podracer (PDF pp. 3-5)
J('4740#1', '30359b#1', "transparent dish stud into the bar's underside, front end (PDF p.3 step 1)")
J('4740#2', '30359b#1', "transparent dish stud into the bar's underside, rear end (PDF p.3 step 1)")
J('30359b#1', '4733#1', "stud brick 1x1 on the front stud of the bar (PDF p.3 step 2)")
J('4733#1', '4070#1', "headlight brick on the side stud of 4733 (PDF p.3 step 2 inset)")
J('4733#1', '6141#7', "round plate on the top stud of 4733 (PDF p.5 step 4)")
J('4733#1', '4592#1', "lever base on a side stud of 4733 (PDF p.3 step 3)")
J('4733#1', '4592#2', "lever base on a side stud of 4733 (PDF p.3 step 3)")
J('4592#1', '4593#2', "lever in its base (PDF p.3 step 3)")
J('4592#2', '4593#1', "lever in its base (PDF p.3 step 3)")
J('30359b#1', '2555#1', "clip tile on the rear stud of the bar (PDF p.3 step 2)")
J('2555#1', '30374#1', "energy bar held in the clip (PDF p.3 step 3)")
for s, (b4, y, bl, gr, co, pm) in {'+': ('4733#2', '6141#1', '6141#4', '6141#5', '4589#2', '2566#2'),
                                   '-': ('4733#3', '6141#2', '6141#3', '6141#6', '4589#1', '2566#1')}.items():
    J('30374#1', b4, "energy bar pushed on the side stud of the pod brick (PDF p.5 step 4)")
    J(b4, y, "yellow round plate on the side stud of the pod brick (PDF p.4 pod step 1)")
    J(b4, pm, "yellow palm-tree top on the side stud of the pod brick (PDF p.4 pod step 3)")
    J(b4, bl, "blue round plate on the side stud of the pod brick (PDF p.4 pod step 2)")
    J(bl, gr, "grey round plate on the blue one (PDF p.4 pod step 2)")
    J(gr, co, "cone on the grey round plate (PDF p.4 pod step 3)")

# ------------------------------------------------------------------ Sebulba's Podracer (PDF pp. 6-12)
J('4740#3', '30359b#2', "transparent dish stud into the bar's underside, front end (PDF p.6 step 1)")
J('4740#4', '30359b#2', "transparent dish stud into the bar's underside, rear end (PDF p.6 step 1)")
J('30359b#2', '4733#4', "stud brick 1x1 on the front stud of the bar (PDF p.6 step 2)")
J('30359b#2', '2555#2', "clip tile on the rear stud of the bar (PDF p.6 step 2)")
J('2555#2', '30374#2', "energy bar held in the clip (PDF p.7 step 3)")
J('4733#4', '4599a#1', "tap on a side stud of 4733 (PDF p.7 step 3)")
J('4733#4', '4599a#2', "tap on a side stud of 4733 (PDF p.7 step 3)")
J('4733#4', '3062b#1', "black round brick on a side stud of 4733 (PDF p.7 step 4)")
J('3062b#1', '4589#3', "cone on the round brick (PDF p.7 step 4 inset)")
J('4733#4', '6141#8', "round plate on the top stud of 4733 (PDF p.6 step 2)")
SIDES = {'+': dict(pod='4733#5', d1='4740#6', d2='4740#7', hd='3626b#2', co='4589#4', ab='4733#7', cl=('2555#3', '2555#4'),
                   asm=[('2555#4', '2540#1', '2420#1', '3023#1', '3024#1'), ('2555#3', '2540#2', '2420#2', '3023#2', '3024#2')]),
         '-': dict(pod='4733#6', d1='4740#5', d2='4740#8', hd='3626b#1', co='4589#5', ab='4733#8', cl=('2555#5', '2555#6'),
                   asm=[('2555#5', '2540#3', '2420#3', '3023#3', '3024#3'), ('2555#6', '2540#4', '2420#4', '3023#4', '3024#4')])}
for s in SIDES.values():
    J('30374#2', s['pod'], "energy bar pushed on the side stud of the pod brick (PDF p.7 step 4 / p.9 step 5)")
    J(s['pod'], s['d1'], "dish on the top stud of the pod brick (PDF p.8 pod step 1)")
    J(s['d1'], s['d2'], "second (orange) dish on the first one (PDF p.8 pod step 2)")
    J(s['d2'], s['hd'], "orange head on the dish stud (PDF p.8 pod step 2)")
    J(s['hd'], s['co'], "grey cone on the head stud (PDF p.8 pod step 3)")
    J(s['pod'], s['ab'], "clip brick on the side stud of the pod brick (PDF p.10 step 6)")
    for c in s['cl']: J(s['ab'], c, "clip tile on a side stud of the clip brick (PDF p.10 step 6 inset)")
    for c, hnd, cor, p12, p11 in s['asm']:
        J(c, hnd, "handle of the black plate clipped in the clip tile (PDF pp.11-12 step 7-8)")
        J(cor, hnd, "orange corner plate on the black plate (PDF pp.11-12 inset step 1)")
        J(cor, p12, "orange corner plate on the 1x2 plate (PDF pp.11-12 inset step 1)")
        J(p11, hnd, "black 1x1 plate on the stud of the black plate (PDF pp.11-12 inset step 2)")

# ------------------------------------------------------------------ decisions
nodes_all = [n.strip('"') for n in list(csv.reader(open('4485_adjacency_matrix_kernel.csv')))[0][1:]]
unk = [n for k in JOINT for n in k if n not in nodes_all]
assert not unk, unk
REM = {'side': "parts only touch sideways (no stud/tube/clip engagement)",
       'near': "boxes do not touch in the real geometry (kernel tolerance too generous)",
       'gap': "vertical gap: parts do not touch",
       'inside': "boxes of a rotated/sideways part overlap but the build sequence shows no joint between these two",
       'stack': "touching only, not engaged (not a stud/tube joint in the instructions)"}
dec = {}
for a, b in E:
    k = pair(a, b)
    if k in JOINT: dec[k] = ('keep', JOINT[k], "geometry + PDF")
    else: dec[k] = ('remove', REM[G.kind(G.rel(a, b))], "geometry + PDF")
for k, why in JOINT.items():
    if k not in dec: dec[k] = ('add', why, "PDF + geometry")
idx = {n: i for i, n in enumerate(nodes_all)}
A = [[0] * len(nodes_all) for _ in nodes_all]
for k, (d, _, _) in dec.items():
    if d in ('keep', 'add'):
        a, b = tuple(k); A[idx[a]][idx[b]] = A[idx[b]][idx[a]] = 1
with open('4485_adjacency_matrix_checked.csv', 'w', newline='') as fh:
    w = csv.writer(fh, quoting=csv.QUOTE_NONNUMERIC); w.writerow([''] + nodes_all)
    for n, r in zip(nodes_all, A): w.writerow([n] + r)
with open('4485_link_audit.csv', 'w', newline='') as fh:
    w = csv.writer(fh); w.writerow(['part_a', 'part_b', 'submodel', 'decision', 'reason', 'evidence', 'in_kernel_matrix'])
    for k, (d, why, ev) in sorted(dec.items(), key=lambda x: (x[1][0], sorted(x[0]))):
        a, b = sorted(k); w.writerow([a, b, P[a]['grp'].split('- ')[-1].replace('.ldr', ''), d, why, ev, int(k in orig)])
c = collections.Counter(d for d, _, _ in dec.values()); tot = sum(map(sum, A)) // 2
print(dict(c), 'checked links', tot, 'kernel', len(orig))
sub = collections.Counter(P[tuple(k)[0]]['grp'] for k, (d, _, _) in dec.items() if d in ('keep', 'add'))
print(sub)
with open('4485_link_audit_summary.md', 'w') as fh:
    fh.write("# 4485 link audit\n\nKernel links: %d; kept %d, removed %d, added %d; checked matrix %d links (%s).\n\n" % (
        len(orig), c['keep'], c['remove'], c['add'], tot, ", ".join("%s: %d" % (k.split('- ')[-1], v) for k, v in sub.items())))
    fh.write("Both podracers are trees (n parts, n-1 joints): every part is added in the instruction steps by one joint, "
             "except the energy bars, which join the clip tile and both pod bricks.\n")
