# 75193 link audit

Starting matrix: 674 instance links. Checked: 113 links (109 kept from kernel, 4 added = false negatives).

Starting links removed as false positives: 565 (of which 323 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 2 (largest 83 of 85 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
(to be written after the booklet cross-check)
