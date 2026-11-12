#!/usr/bin/env python3
"""Add the two 298c04 levers (missing from the MPD) to 4010_adjacency_matrix.csv.
Each lever connects only to part 4289 (the mast). Idempotent. Run after run_adj.R, before run_plot.R."""
import csv
f = "4010_adjacency_matrix.csv"
rows = list(csv.reader(open(f, encoding="utf-8")))
hdr = rows[0][1:]
if not any(h.startswith("298c04#") for h in hdr):
    new = ["298c04#1", "298c04#2"]
    t = [h for h in hdr if h.split("#")[0] == "4289"]
    assert t, "4289 not found"
    names = hdr + new
    A = {r[0]: dict(zip(hdr, r[1:])) for r in rows[1:]}
    out = [[""] + names]
    for n in names:
        row = []
        for m in names:
            if n in new and m in t or m in new and n in t: row.append("1")
            elif n in new or m in new: row.append("0")
            else: row.append(A[n][m])
        out.append([n] + row)
    with open(f, "w", newline="", encoding="utf-8") as fh:
        csv.writer(fh, quoting=csv.QUOTE_NONNUMERIC).writerows(
            [[out[0][0]] + out[0][1:]] + [[r[0]] + [int(x) for x in r[1:]] for r in out[1:]])
    print("added", new, "->", t)
else:
    print("levers already present")
