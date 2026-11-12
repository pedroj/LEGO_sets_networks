# 4010 adjacency matrix - cross-check (2026-11-11)

Checks every link in `../4010_adjacency_matrix.csv` against the real LDraw part
geometry of `../4010_-_Police_rescue_boat.mpd`, and against the building
instructions (letsbuilditagain.com scan, steps 1-11 + roof sub-steps 1-4).

`check_links.py MPD MATRIX LDRAW_DIR OUT_DIR`
- loads each part's true shape and stud positions from the LDraw library
- a link is *supported* when a stud of one part sits inside the other, or when it
  is a known non-stud joint (minifig joints; glass in window frame, searchlight /
  tap in railing open stud, dish on tap, hinge tile on sunroof, airtanks on bar -
  all visible in the instructions)
- otherwise it is labelled by geometry: apart / side by side / stacked, no stud /
  boxes intersect

## Result (316 links)
| assessment | n | why |
|---|---|---|
| supported | 96 | 66 stud couplings + 30 joints |
| false positive | 219 | 148 parts >= 3 LDU apart (checked on surfaces too), 51 only side by side, 5 resting without studs, 15 overlapping but not joined (mostly minifig parts) |
| uncertain | 1 | 3829c01#1 - 3820#4: hand may grip the steering wheel |

Missing links (stud couplings not in the matrix): 18 - see `4010_missing_links.csv`
(windows and bow railings on the hull deck, 3665 inverted slopes under 3939,
1x8 plates on 1x1/1x2 plates, lenses on the searchlights, light on the mast ...).

## Files
- `4010_link_check.csv` - every matrix link with verdict, assessment and reason
- `4010_missing_links.csv` - stud couplings the matrix lacks
- `4010_adjacency_matrix_checked.csv` - matrix with false positives removed and
  missing links added (115 links; one connected structure + the loose helmet 3842a)
- `4010_parts_geometry.csv` - part positions and stud counts
