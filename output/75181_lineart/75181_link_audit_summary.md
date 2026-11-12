# 75181 link audit

Kernel: 15816 instance links. Checked: 6005 links (3481 kept from kernel, 2524 added = false negatives).

Kernel links removed as false positives: 12335 (of which 5988 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 26 (largest 1746 of 1802 parts). Parts with no link to an earlier-built part: 97.

Rules: stud engagement (box contact along any axis within -1..5.5 LDU with >=8 LDU footprint, SNOT included); non-stud joints (pin, axle, bar, clip, ball, hinge...) when boxes touch/interpenetrate and the subtype matrix allows the joint; isolated parts keep kernel links that touch in LDraw geometry.
Build order = instance expansion order in the MPD; the instruction PDF was used for sub-assembly sampling only.
