# 4010 link audit (kernel matrix vs LDraw geometry + building instructions)

Kernel links: 318 (incl. 2 levers). Kept 100, removed 218, added 19 -> checked matrix 119 links.

## Removed (by reason)
- 118 x bounding boxes not in contact in real LDraw geometry (kernel box too large)
- 34 x parts only touch sideways (no stud/tube/clip engagement)
- 34 x minifig parts touch but are not joined (only head-torso, hips-torso/legs, arm-torso, hand-arm, cap-head, hand-radio join)
- 30 x vertical gap: parts do not touch
- 2 x volumes overlap but no real joint between these parts

## Added (real stud/clip/rest contacts the kernel missed)
- 2372c01#1 - 3842a#1: loose diving helmet standing on a cockpit-floor stud next to the airtanks (PDF step 6)
- 2372c01#1 - 4863#1: window frame rests on hull deck (PDF step 7)
- 2372c01#1 - 4863#2: window frame rests on hull deck (PDF step 7)
- 2372c01#1 - 4873#2: bar 1x6 at bow rests on hull deck (PDF step 11)
- 3004#4 - 3829c01#1: steering stand 1x2 stands on brick 1x2 (PDF steps 4-6)
- 3023#2 - 3460#1: 1x2 plate on 1x8 plate (PDF step 5-6)
- 3023#3 - 3460#1: 1x2 plate on 1x8 plate (PDF step 5-6)
- 3023#4 - 4863#2: plate on window frame (PDF steps 6-7)
- 3023#5 - 4863#1: plate on window frame (PDF steps 6-7)
- 3024#1 - 3460#1: plate 1x8 on the 1x1 plates (PDF step 5-6)
- 3024#2 - 3460#1: plate 1x8 on the 1x1 plates (PDF step 5-6)
- 3460#2 - 4286#1: 1x8 plate on 3x1 slopes (PDF step 8)
- 3460#2 - 4286#2: 1x8 plate on 3x1 slopes (PDF step 8)
- 3665#1 - 3939#1: slope 3x6 rests on the inverted slopes (PDF step 6)
- 3665#2 - 3939#1: slope 3x6 rests on the inverted slopes (PDF step 6)
- 4073#1 - 4349#1: round plate 4073 at loudhailer end (PDF step 11 inset)
- 4073#2 - 4349#2: round plate 4073 at loudhailer end (PDF step 11 inset)
- 4289#1 - 6141#2: round plate on mast top (PDF step 11 inset)
- 4599a#1 - 6141#1: round plate on tap (PDF step 6 inset)

Files: check_geometry.py, check_with_instructions.py, 4010_link_audit.csv, 4010_adjacency_matrix_checked.csv, run_adj_checked.R