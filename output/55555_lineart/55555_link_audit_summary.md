# 55555 link audit

Starting matrix: 86 instance links. Checked: 48 links (27 kept from kernel, 21 added = false negatives).

Starting links removed as false positives: 59 (of which 41 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 2 (largest 40 of 41 parts). Isolated parts: 1.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
(to be written after the booklet cross-check)
