# 75257 link audit

Starting matrix: 9518 instance links. Checked: 2158 links (1504 kept from kernel, 654 added = false negatives).

Starting links removed as false positives: 8014 (of which 4525 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 49 (largest 1259 of 1383 parts). Isolated parts: 19.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
(to be written after the booklet cross-check)
