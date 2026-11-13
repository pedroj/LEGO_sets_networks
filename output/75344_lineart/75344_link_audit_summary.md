# 75344 link audit

Starting matrix: 97 instance links. Checked: 97 links (97 kept from kernel, 0 added = false negatives).

Starting links removed as false positives: 0 (of which 0 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 4 (largest 65 of 78 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
Ported to the shared pipeline (13 Nov 2026). Old audit used a bespoke stack/inside rule plus the .cys edge list as tie-breaker; the shared strict stud rule replaces it.
Old disputed pairs (not added): 11458-15068 (>50 LDU apart), 23950-32064a (4 LDU gap), 2419-3020 (side by side), 24307-32952 (side contact only).
