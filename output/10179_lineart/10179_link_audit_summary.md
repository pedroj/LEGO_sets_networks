# 10179 link audit

Starting matrix: 29674 instance links. Checked: 7665 links (5101 kept from kernel, 2564 added = false negatives).

Starting links removed as false positives: 24573 (of which 12285 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 126 (largest 4199 of 5148 parts). Isolated parts: 19.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
(to be written after the booklet cross-check)
