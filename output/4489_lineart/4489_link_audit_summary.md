# 4489 link audit

Starting matrix: 345 instance links. Checked: 93 links (83 kept from kernel, 10 added = false negatives).

Starting links removed as false positives: 262 (of which 154 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 1 (largest 82 of 82 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 1 links were added and 14 removed by hand after comparing the geometry with the instruction pictures.
Hand checks covered the whole booklet (body steps 1-16, leg sub-assembly, head steps 1-6) against the MPD STEP lines.
Added joints: head pin 43093-30553, pins 30000-75535, leg axle 32062 through 30552 and 4032a, round plate 6141#3 on clip plate 4081b#1.
14 flush-contact false positives removed (handle plates 2540, hinge bricks 30365, pin connectors 75535). 3678a reclassified as a slope, 30000 as a brick with pins.
