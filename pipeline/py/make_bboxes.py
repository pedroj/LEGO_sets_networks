"""Local bounding box (LDraw units, part origin = LDraw origin) of every part in the model, from LDraw geometry.
Usage (inside output/<set>_lineart/): python3 ../../pipeline/py/make_bboxes.py parts_list.txt part_bbox.csv"""
import csv, sys, numpy as np
import lego_lineart as L
import os
L.LOCAL = os.environ.get("LDRAW_DIR", "/Users/pedro/Documents/LEGO Creations/Utils/ldraw")
parts = L.read_parts(sys.argv[1]); rows = []
for p in parts:
    f, d, c = L.resolve(p)
    if f is None: print("not found", p); continue
    L.prefetch(f + ".dat"); tris, lines = [], []
    L.collect(f + ".dat", np.eye(4), tris, lines)
    pts = np.vstack(tris) if tris else np.vstack([l for l, _ in lines])
    mn, mx = pts.min(0), pts.max(0)
    rows.append([p] + [round(float(v), 2) for v in (mn[0], mx[0], mn[1], mx[1], mn[2], mx[2])])
    print(p, f, rows[-1][1:])
with open(sys.argv[2], "w", newline="") as fh:
    w = csv.writer(fh); w.writerow("part xmin xmax ymin ymax zmin zmax".split()); w.writerows(rows)
