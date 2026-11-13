# CELEB2015MF link audit

Starting matrix: 42 instance links. Checked: 15 links (10 kept from kernel, 5 added = false negatives).

Starting links removed as false positives: 32 (of which 27 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 1 (largest 16 of 16 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
Pedro approved 13 Nov 2026. The PDF for this set is a Rebrickable web page (photo + parts list), not building instructions, so the checks rest on geometry. 3023b (missing in Rebrickable) classed as Plate; tap 4599b as Brick basic; binoculars 30162 allowed on the stud below them.
