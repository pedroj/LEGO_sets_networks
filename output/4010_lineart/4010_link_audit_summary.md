# 4010 link audit

Starting matrix: 121 instance links. Checked: 121 links (121 kept from kernel, 0 added = false negatives).

Starting links removed as false positives: 0 (of which 0 are candidate pairs failing the geometry rules; the rest are pairs more than 6 LDU apart).

Connected components: 1 (largest 94 of 94 parts). Isolated parts: 0.

Rules: stud engagement (stud insertion of 2-5.5 LDU along an axis with >=8 LDU footprint; flush side contact is not a joint); non-stud joints (hinge, pin, axle, bar, clip...) when the boxes touch and the subtype matrix allows the joint. 45 links were added and 5 removed by hand after comparing the geometry with the instruction pictures.
Ported to the shared pipeline (13 Nov 2026). Starting matrix = the old checked matrix (hull rests on deck/cockpit-floor studs, minifig joins, glass/hinge/clip whitelist, 19 manual adds).
Hull: the MPD hull is a custom triangle sub-model, replaced by the official LDraw part 2372c01 (mpd_block_alias). Hull links come from the starting matrix and manual_add.
Levers 298c04 are not in the MPD (extra_lines) nor in LDraw (flexible_parts); they are linked by manual_add.
