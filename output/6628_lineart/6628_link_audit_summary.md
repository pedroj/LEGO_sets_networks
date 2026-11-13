# 6628 link audit

Starting matrix: 56 instance links. Checked: 56 links (56 kept from kernel, 0 added = false negatives).

Starting links removed as false positives: 0 (of which 0 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 1 (largest 40 of 40 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 9 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
Hand checks covered steps 1-8 and the roof sub-assembly: doors, steering wheel, tow hook, ladder plate, plate inside the crane slope.
The MPD lists parts by position (no STEP lines), so it gives no build order; the instruction pictures were used instead. 3 vehicle parts were reclassified (4211 base and 3788 mudguard as bricks, 3136 hook as plate with studs); the tyre-on-wheel joint 122c01-4084 was allowed.
