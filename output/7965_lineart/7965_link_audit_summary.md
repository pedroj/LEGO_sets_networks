# 7965 link audit

Starting matrix: 8028 instance links. Checked: 2024 links (1360 kept from kernel, 664 added = false negatives).

Starting links removed as false positives: 6668 (of which 3460 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 25 (largest 1184 of 1288 parts). Isolated parts: 3.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
her.dat (1 instance in the Studio/LDraw export) is an MLCad rotation helper, not a part (confirmed by Pedro); removed from the set's MPD copy before the analysis.
(booklet cross-check still to be written)
