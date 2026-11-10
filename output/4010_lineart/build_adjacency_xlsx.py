#!/usr/bin/env python3
"""4010 part-type adjacency matrix as Excel with part icons (layout as 75344-1_adjacency_matrix.xlsx).
Run from this folder: python3 build_adjacency_xlsx.py  (needs openpyxl, pillow; icons/ and 4010_nodes.csv/_edges.csv from plot_lego_network.R)"""
import csv, os, collections
from PIL import Image as PILImage
from openpyxl import Workbook
from openpyxl.drawing.image import Image
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter as L

PRE = "4010"; SET_TITLE = "LEGO 4010-1 Police Rescue Boat"
H = os.path.dirname(os.path.abspath(__file__))
rd = lambda f: list(csv.DictReader(open(os.path.join(H, f), encoding="utf-8")))
nodes = rd(PRE + "_nodes.csv"); edges = rd(PRE + "_edges.csv")
sub_order = [r["subtype"] for r in csv.DictReader(open(os.path.join(H, "../../data/lego_compat_out/subtypes_interfaces.csv")))] + ["Other"]
CATS = ["Brick", "Plate", "Tile", "Slope", "Baseplate", "Panel", "Technic", "Special", "Minifig", "Other"]
CAT = {"Brick": "F4B8B2", "Plate": "D3E0F5", "Tile": "E6D9BE", "Slope": "FFD3A8", "Baseplate": "C0DDBF", "Panel": "C5D6E8",
       "Technic": "D0D1D3", "Special": "DDBFDA", "Minifig": "F2E0A0", "Other": "E0E0E0"}
nodes.sort(key=lambda r: (CATS.index(r["category"]), sub_order.index(r["subtype"]) if r["subtype"] in sub_order else 99, r["part"]))
parts = [r["part"] for r in nodes]; info = {r["part"]: r for r in nodes}

# collapse instance matrix (3004#1 ...) to part types
m = list(csv.reader(open(os.path.join(H, PRE + "_adjacency_matrix.csv"), encoding="utf-8")))
ids = [x.split("#")[0] for x in m[0][1:]]
W = collections.defaultdict(float)
for i in range(len(ids)):
    for j in range(i + 1, len(ids)):
        v = float(m[1 + i][1 + j] or 0)
        if v > 0: W[tuple(sorted((ids[i], ids[j]))) if ids[i] != ids[j] else (ids[i], ids[i])] += v
# consistency check with the edge table
for e in edges:
    k = tuple(sorted((e["from"], e["to"]))); assert abs(W[k] - float(e["contacts_proximity"])) < 1e-9, (k, W[k], e)
check = {tuple(sorted((e["from"], e["to"]))) for e in edges if e["system"] == "check"}
JOINT = {}
for e in edges:
    JOINT[tuple(sorted((e["from"], e["to"])))] = e["system"]

fill = lambda h: PatternFill("solid", fgColor=h)
thin = Side(style="thin", color="BFBFBF"); box = Border(left=thin, right=thin, top=thin, bottom=thin)
F9 = Font(size=9); F9B = Font(size=9, bold=True)
os.makedirs(os.path.join(H, "icons_small"), exist_ok=True)
def icon(p):
    src = os.path.join(H, "icons", p + ".png"); dst = os.path.join(H, "icons_small", p + ".png")
    if not os.path.exists(src): return None
    if not os.path.exists(dst): PILImage.open(src).convert("RGBA").resize((156, 156), PILImage.LANCZOS).save(dst)
    im = Image(dst); im.width = im.height = 52; return im

wb = Workbook(); ws = wb.active; ws.title = "Adjacency"
n = len(parts); R0, C0 = 8, 6
ws["A1"] = f"{SET_TITLE}: part-type adjacency matrix"; ws["A1"].font = Font(size=13, bold=True)
ws["A2"] = ("Cell = number of instance-level contacts between two part types (from 4010_adjacency_matrix.csv, instances collapsed). "
            "Blank = no recorded contact. Diagonal = same part type joined to itself. Light red = contact that the compatibility rules say cannot be a "
            "direct joint (false positive: not drawn in the network, weight 0 in 4010_edges.csv). Icons rendered from the LDraw library.")
ws["A2"].font = Font(size=9, italic=True)
for r, lab in ((3, "Icon"), (4, "Part"), (5, "Name"), (6, "Subtype"), (7, "Category / Part →")):
    c = ws.cell(r, 5, lab); c.font = F9B; c.alignment = Alignment(horizontal="right", vertical="center")
for c, h in zip(range(1, 6), ("Icon", "Part", "Name", "Subtype", "Category")):
    x = ws.cell(7, c, h); x.font = F9B; x.fill = fill("EEEEEE")
ws.cell(7, 5, "Category / Part →")
for i, p in enumerate(parts):
    d = info[p]; cf = fill(CAT[d["category"]]); col = C0 + i; row = R0 + i
    for r, v, rot in ((4, p, 0), (5, d["desc"], 90), (6, d["subtype"], 90), (7, d["category"], 0)):
        c = ws.cell(r, col, v); c.font = F9B if r == 4 else F9; c.fill = cf; c.border = box
        c.alignment = Alignment(horizontal="center", vertical="bottom" if rot else "center", textRotation=rot, wrap_text=bool(rot))
    for c, v in ((2, p), (3, d["desc"]), (4, d["subtype"]), (5, d["category"])):
        x = ws.cell(row, c, v); x.font = F9B if c == 2 else F9; x.fill = cf; x.border = box
        x.alignment = Alignment(vertical="center", horizontal="center" if c in (2, 5) else None, wrap_text=c in (3, 4))
    im = icon(p)
    if im: ws.add_image(im, f"{L(col)}3"); ws.add_image(icon(p), f"A{row}")
    for j, q in enumerate(parts):
        k = (p, q) if p == q else tuple(sorted((p, q)))
        v = W.get(k); c = ws.cell(row, C0 + j, int(v) if v else None); c.border = box
        c.font = F9; c.alignment = Alignment(horizontal="center", vertical="center")
        if p == q: c.fill = fill("F2F2F2")
        elif v and k in check: c.fill = fill("F4B6B6")
ws.row_dimensions[3].height = 42; ws.row_dimensions[4].height = 15
ws.row_dimensions[5].height = 190; ws.row_dimensions[6].height = 150; ws.row_dimensions[7].height = 22
for r in range(R0, R0 + n): ws.row_dimensions[r].height = 42
for c, w in zip("ABCDE", (8.5, 9, 38, 28, 17)): ws.column_dimensions[c].width = w
for j in range(n): ws.column_dimensions[L(C0 + j)].width = 8.5
ws.freeze_panes = ws.cell(R0, C0)

s2 = wb.create_sheet("Parts")
hdr = ["Icon", "Part", "Name", "Category", "Subtype", "Instances", "Linked part types (drawn)", "Strength (drawn contacts)"]
deg = collections.Counter(); st = collections.Counter()
for e in edges:
    if e["system"] != "check":
        deg[e["from"]] += 1; deg[e["to"]] += 1; st[e["from"]] += float(e["weight"]); st[e["to"]] += float(e["weight"])
for c, h in enumerate(hdr, 1):
    x = s2.cell(1, c, h); x.font = F9B; x.fill = fill("DDDDDD"); x.border = box
for i, p in enumerate(parts):
    d = info[p]; r = i + 2
    for c, v in enumerate(["", p, d["desc"], d["category"], d["subtype"], int(d["instances"]), deg[p], int(st[p])], 1):
        x = s2.cell(r, c, v if v != "" else None); x.font = F9; x.border = box; x.alignment = Alignment(vertical="center", wrap_text=c in (3, 5))
        if c in (4, 5): x.fill = fill(CAT[d["category"]])
    s2.row_dimensions[r].height = 42
    im = icon(p)
    if im: s2.add_image(im, f"A{r}")
for c, w in zip("ABCDEFGH", (8.5, 9, 48, 11, 32, 10, 14, 14)): s2.column_dimensions[c].width = w
s2.freeze_panes = "B2"

s3 = wb.create_sheet("Edges")
cols = ["from", "to", "weight", "system", "contacts_proximity"]
for c, h in enumerate(cols, 1): s3.cell(1, c, h).font = F9B
for i, e in enumerate(edges, 2):
    for c, h in enumerate(cols, 1):
        v = e[h]; s3.cell(i, c, float(v) if h in ("weight", "contacts_proximity") else v).font = F9
        if e["system"] == "check": s3.cell(i, c).fill = fill("F4B6B6")
for c, w in zip("ABCDE", (10, 10, 9, 12, 18)): s3.column_dimensions[c].width = w
out = os.path.join(H, f"{PRE}_adjacency_matrix_icons.xlsx"); wb.save(out)
print("saved", out, "| part types", n, "| pairs", sum(1 for k in W if k[0] != k[1]), "| check pairs", len(check))
