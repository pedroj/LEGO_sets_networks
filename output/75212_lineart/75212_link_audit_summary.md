# 75212 link audit

Starting matrix: 9271 instance links. Checked: 2277 links (1500 kept from kernel, 777 added = false negatives).

Starting links removed as false positives: 7771 (of which 3795 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 31 (largest 1303 of 1376 parts). Isolated parts: 4.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
(to be written after the booklet cross-check)
