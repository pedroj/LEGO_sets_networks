#!/usr/bin/env python3
"""Upward-facing surfaces of the LDraw hull 2372c01 -> hull_up_faces.csv (read by run_adj.R).
Columns: xmin,xmax,zmin,zmax,y of every upward-facing triangle (LDraw y points down, so up = -y)."""
import sys, csv, collections, os
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import lego_lineart as L
L.LOCAL = "/Users/pedro/Documents/LEGO Creations/Utils/ldraw"
tris, lines = [], []
L.collect("2372c01.dat", np.eye(4), tris, lines)
T = np.array(tris); n = np.cross(T[:, 1] - T[:, 0], T[:, 2] - T[:, 0]); nn = np.linalg.norm(n, axis=1); a = nn / 2
up = (n[:, 1] < -0.98 * nn) & (a > 2)
rows = [(T[i][:, 0].min(), T[i][:, 0].max(), T[i][:, 2].min(), T[i][:, 2].max(), T[i][:, 1].mean()) for i in np.where(up)[0]]
out = os.path.join(os.path.dirname(os.path.abspath(__file__)), "hull_up_faces.csv")
with open(out, "w", newline="") as f:
    w = csv.writer(f); w.writerow(["xmin", "xmax", "zmin", "zmax", "y"]); w.writerows([[round(v, 2) for v in r] for r in rows])
print(len(rows), "faces;", sorted(collections.Counter(round(r[4]) for r in rows).items(), key=lambda t: -t[1])[:4])
