# 4485 link audit

Starting matrix: 65 instance links. Checked: 65 links (65 kept from kernel, 0 added = false negatives).

Starting links removed as false positives: 0 (of which 0 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 2 (largest 43 of 67 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 65 links were added and 21 removed by hand after comparing the geometry with the instruction pictures.
