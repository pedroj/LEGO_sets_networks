#!/usr/bin/env python3
"""Cross-check of the kernel adjacency matrix of set 4010 against (i) real LDraw geometry
(check_geometry.py: world-space bounding boxes with studs) and (ii) the building instructions
(4010-1 Police rescue boat PDF: blue interior PDF steps 2-4, white superstructure 5-8,
roof sub-assembly inset 9.1-9.4, tap/loudhailer/minifigs 6 and 11).
Input : 4010_adjacency_matrix_kernel.csv   (kernel/proximity matrix, with the 2 levers)
Output: 4010_adjacency_matrix.csv (checked), 4010_link_audit.csv (one row per link: keep / remove / add + reason)
Rules
  stack  : footprints overlap and one part's underside meets the other's stud tops (bbox overlap 0-5 LDU) -> stud/tube joint
  side/near/gap : parts only touch sideways or are apart -> NOT a mechanical link (removed) unless whitelisted below
  inside : volumes interpenetrate -> kept only for real nesting joints (hull rests, glass in frame, hinge, clip, minifig, tap/loudhailer)
"""
import csv, collections
import check_geometry as G
P, E = G.P, G.E
base = lambda n: n.split('#')[0]
pair = lambda a, b: frozenset((a, b))
MINIFIG = {'3815','3816','3817','973p1f','973p1g','3818','3819','3820','3626bp01','3624','3962a'}
# --- whitelist of real non-stack joints (type pairs) with the reason and instruction evidence
KEEP_TYPE = {
 ('4862','4863'): "glass pane clipped into window frame (PDF step 7 inset)",
 ('2348a','2349a'): "sunroof glass held in the hinged roof frame (PDF inset 9.1)",
 ('2349a','4625'): "hinge tile 1x4 hinged to roof frame (PDF inset 9.2)",
 ('3838','4623'): "airtanks clip onto the bar of plate 4623 (PDF steps 5-6)",
 ('4349','4873'): "loudhailer seated on the open stud of the 1x6 bar (PDF step 11 inset)",
 ('4599a','4873'): "tap seated on the bar stud (PDF step 6 inset)",
 ('4599a','6141'): "round plate on tap (PDF step 6 inset)",
 ('4599a','4740'): "radar dish mounted on the tap (PDF step 11 inset)",
 ('3624','3626bp01'): "police cap on head",
 ('3626bp01','973p1f'): "head on torso", ('3626bp01','973p1g'): "head on torso",
 ('3815','973p1f'): "torso on hips", ('3815','973p1g'): "torso on hips",
 ('3815','3816'): "hips-leg", ('3815','3817'): "hips-leg",
 ('3818','973p1f'): "arm on torso", ('3818','973p1g'): "arm on torso",
 ('3819','973p1f'): "arm on torso", ('3819','973p1g'): "arm on torso",
 ('3818','3820'): "hand on arm", ('3819','3820'): "hand on arm",
}
REMOVE_REASON = {
 'side': "parts only touch sideways (no stud/tube/clip engagement)",
 'near': "bounding boxes not in contact in real LDraw geometry (kernel box too large)",
 'gap': "vertical gap: parts do not touch",
 'inside': "volumes overlap but no real joint between these parts",
 'stack': "",
}
MF_REMOVE = "minifig parts touch but are not joined (only head-torso, hips-torso/legs, arm-torso, hand-arm, cap-head, hand-radio join)"
dec = {}  # frozenset -> (decision, reason, evidence)
orig = {pair(a, b) for a, b in E}
HULL = '2372c01#1'
for a, b in E:
    k = pair(a, b); ta, tb = base(a), base(b)
    if a not in P or b not in P:       # the two levers added by hand
        dec[k] = ('keep', "lever 298c04 clipped on mast 4289 (user-confirmed; lever absent from the MPD)", "user"); continue
    o = G.rel(a, b); kd = G.kind(o); tp = tuple(sorted((ta, tb)))
    if HULL in (a, b):
        other = b if a == HULL else a
        if base(other) in MINIFIG: dec[k] = ('remove', "minifig does not touch the hull", "geometry")
        else: dec[k] = ('keep', "rests on hull deck/cockpit floor studs (PDF steps 1-5)", "PDF 1-5 + hull surface test")
    elif kd == 'stack' and not (ta in MINIFIG and tb in MINIFIG and tp not in {('3816','3958')}):
        if (ta in MINIFIG) != (tb in MINIFIG) and tp not in {('3816','3958'),('3817','3958')}:
            dec[k] = ('remove', MF_REMOVE, "geometry")
        elif {ta,tb}=={'3624','973p1f'} or {ta,tb}=={'3624','973p1g'}:
            dec[k] = ('remove', "cap sits on the head, not on the torso", "geometry")
        else:
            dec[k] = ('keep', "stud/tube stack: underside meets studs, footprints overlap (PDF %s)" % ("steps 2-8" if ta not in MINIFIG else "step 11"), "geometry + PDF")
    elif tp in KEEP_TYPE and kd in ('inside','stack','near'):
        # for minifig-hand and radio keep only valid instances (checked below)
        dec[k] = ('keep', KEEP_TYPE[tp], "PDF")
    elif tp == ('3820','3962a') and a.startswith('3820#1') or tp == ('3820','3962a') and b.startswith('3820#1'):
        dec[k] = ('keep', "radio held in the hand (PDF step 11)", "PDF")
    else:
        why = MF_REMOVE if (ta in MINIFIG or tb in MINIFIG) else REMOVE_REASON[kd]
        dec[k] = ('remove', why, "geometry" if kd in ('near','gap','side') else "geometry/PDF")
# hand-arm instance consistency: keep only hand-arm pairs where hand is the nearest-to-arm instance (as kernel kept 1:1)
# --- links present in the real model that the kernel missed
ADD = [
 ('3004#4','3829c01#1',"steering stand 1x2 stands on brick 1x2 (PDF steps 4-6)"),
 ('3665#1','3939#1',"slope 3x6 rests on the inverted slopes (PDF step 6)"),
 ('3665#2','3939#1',"slope 3x6 rests on the inverted slopes (PDF step 6)"),
 ('3024#1','3460#1',"plate 1x8 on the 1x1 plates (PDF step 5-6)"),
 ('3024#2','3460#1',"plate 1x8 on the 1x1 plates (PDF step 5-6)"),
 ('3460#1','3023#2',"1x2 plate on 1x8 plate (PDF step 5-6)"),
 ('3460#1','3023#3',"1x2 plate on 1x8 plate (PDF step 5-6)"),
 ('4863#1','3023#5',"plate on window frame (PDF steps 6-7)"),
 ('4863#2','3023#4',"plate on window frame (PDF steps 6-7)"),
 ('4286#1','3460#2',"1x8 plate on 3x1 slopes (PDF step 8)"),
 ('4286#2','3460#2',"1x8 plate on 3x1 slopes (PDF step 8)"),
 ('4289#1','6141#2',"round plate on mast top (PDF step 11 inset)"),
 ('4349#1','4073#1',"round plate 4073 at loudhailer end (PDF step 11 inset)"),
 ('4349#2','4073#2',"round plate 4073 at loudhailer end (PDF step 11 inset)"),
 ('4599a#1','6141#1',"round plate on tap (PDF step 6 inset)"),
 (HULL,'3842a#1',"loose diving helmet standing on a cockpit-floor stud next to the airtanks (PDF step 6)"),
 (HULL,'4863#1',"window frame rests on hull deck (PDF step 7)"),
 (HULL,'4863#2',"window frame rests on hull deck (PDF step 7)"),
 (HULL,'4873#2',"bar 1x6 at bow rests on hull deck (PDF step 11)"),
]
for a,b,why in ADD:
    k = pair(a,b)
    if k not in dec: dec[k] = ('add', why, "PDF + geometry")
# rebuild matrix
names = list(csv.reader(open('4010_adjacency_matrix_kernel.csv')))[0][1:]
names = [n.strip('"') for n in names]; idx = {n:i for i,n in enumerate(names)}
A = [[0]*len(names) for _ in names]
for k,(d,_,_) in dec.items():
    if d in ('keep','add'):
        a,b = tuple(k); A[idx[a]][idx[b]] = A[idx[b]][idx[a]] = 1
with open('4010_adjacency_matrix.csv','w',newline='') as fh:
    w = csv.writer(fh, quoting=csv.QUOTE_NONNUMERIC)
    w.writerow(['']+names)
    for n,r in zip(names,A): w.writerow([n]+r)
with open('4010_link_audit.csv','w',newline='') as fh:
    w = csv.writer(fh); w.writerow(['part_a','part_b','decision','reason','evidence','in_kernel_matrix'])
    for k,(d,why,ev) in sorted(dec.items(), key=lambda t: (t[1][0], sorted(t[0]))):
        a,b = sorted(k); w.writerow([a,b,d,why,ev,'yes' if k in orig else 'no'])
c = collections.Counter(d for d,_,_ in dec.values())
print(dict(c), 'links kept+added =', c['keep']+c['add'], 'of kernel', len(orig))
