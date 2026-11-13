# 75257 link audit

Starting matrix: 9973 instance links. Checked: 2196 links (1539 kept from kernel, 657 added = false negatives).

Starting links removed as false positives: 8434 (of which 4653 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 47 (largest 1261 of 1383 parts). Isolated parts: 19.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
(to be written after the booklet cross-check)
