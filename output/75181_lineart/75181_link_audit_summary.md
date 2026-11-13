# 75181 link audit

Starting matrix: 3040 instance links. Checked: 3040 links (3040 kept from kernel, 0 added = false negatives).

Starting links removed as false positives: 0 (of which 0 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 42 (largest 1690 of 1802 parts). Isolated parts: 8.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
Ported to the shared pipeline (strict stud rule 2-5.5 LDU). Starting matrix = current curated 75181_adjacency_matrix.csv.
