# 10174 link audit

Starting matrix: 1613 instance links. Checked: 1613 links (1613 kept from kernel, 0 added = false negatives).

Starting links removed as false positives: 0 (of which 0 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 5 (largest 1033 of 1062 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 0 links were added and 0 removed by hand after comparing the geometry with the instruction pictures.
No building-instruction booklet was supplied, so no links were checked by hand against pictures.
The three flexible hoses (73590c02a) are not box-checked: their kernel links (segment by segment) are kept. Sub-assemblies cut off by the rules are bridged by touching kernel links.
NOTE: 10174_instances.csv / _kernel.csv come from output/10174_lineart/run_adj.R (custom MPD sub-files, hose collapsing, segment-wise hose contacts), which the shared instances step does not reproduce. Keep run_adj.R if the instances must be regenerated.
