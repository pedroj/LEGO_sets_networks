# 10174 link audit (LEGO 10174-1 Imperial AT-ST)

Kernel: 5343 instance links. Checked: 1613 links (944 kept from kernel, 669 added = false negatives).

Kernel links removed as false positives: 4399 (of which 2684 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 5 (largest 1033 of 1062 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush face or side contact is not a joint); non-stud joints (pin, axle, hinge, bar...) when the boxes touch and the subtype matrix allows the joint; isolated parts keep geometry-confirmed kernel links; sub-assemblies cut off by the rules are bridged by touching kernel links (one round per component pair). The three flexible hoses (73590c02a) are not box-checked: their kernel links (segment by segment) are kept.

No building-instruction booklet was supplied, so no links were checked by hand against pictures.
