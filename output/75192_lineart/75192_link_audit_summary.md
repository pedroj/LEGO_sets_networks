# 75192 link audit

Starting matrix: 48327 instance links. Checked: 10574 links (7103 kept from kernel, 3471 added = false negatives).

Starting links removed as false positives: 41224 (of which 21663 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 219 (largest 6031 of 7415 parts). Isolated parts: 29.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
(to be written after the booklet cross-check)
