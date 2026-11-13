# 75193 link audit

Starting matrix: 674 instance links. Checked: 120 links (112 kept from kernel, 8 added = false negatives).

Starting links removed as false positives: 562 (of which 320 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 1 (largest 85 of 85 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 1 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
Pedro approved 13 Nov 2026. Booklet step 39: jumper 15573 + round plate 85861 + pneumatic T-piece 4697b form the top turret; the T bar sits in the open stud of 85861 (manual_add). 3023b classed as Plate; trigger 15392 as minifig accessory.
