#!/usr/bin/env python3
"""Excel version of compat_matrix_subtype_codes.csv with part icons (layout as 75344-1_adjacency_matrix.xlsx).
Run from this folder:  python3 build_compat_xlsx.py   (needs openpyxl, pillow; icons/ from lego_lineart.py)"""
import csv, os, sys
from PIL import Image as PILImage
from openpyxl import Workbook
from openpyxl.drawing.image import Image
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter as L
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from reps import REPS

HERE = os.path.dirname(os.path.abspath(__file__))
codes = list(csv.reader(open(os.path.join(HERE, "compat_matrix_subtype_codes.csv"), encoding="utf-8")))
names = codes[0][1:]
cell = {r[0]: dict(zip(names, r[1:])) for r in codes[1:]}
sub = {r["subtype"]: r for r in csv.DictReader(open(os.path.join(HERE, "subtypes_interfaces.csv"), encoding="utf-8"))}
desc = {r["part_num"]: r["description"].lstrip("~=_| ") for r in csv.DictReader(open(os.path.join(HERE, "icons/manifest.csv")))}
order = list(sub.keys())                       # project order (category blocks)
assert set(order) == set(names), set(order) ^ set(names)

PRI = ["pin", "axle", "ball", "hinge", "turntable", "clip", "bar", "wheel", "gear", "track", "door", "minifig", "stud"]
JOINT = {"stud": "D0D0D0", "bar": "F9C890", "clip": "F2A9A8", "pin": "9FD6CF", "axle": "B7DDA9", "ball": "F5E39B",
         "hinge": "D9BDD3", "turntable": "FFCED2", "gear": "D4BCAF", "track": "C4C4C4", "wheel": "9FD9C5",
         "door": "ECB48F", "minifig": "BAB6DB"}
CAT = {"Brick": "F4B8B2", "Plate": "D3E0F5", "Tile": "E6D9BE", "Slope": "FFD3A8", "Baseplate": "C0DDBF",
       "Panel": "C5D6E8", "Technic": "D0D1D3", "Special": "DDBFDA", "Minifig": "F2E0A0"}
fill = lambda h: PatternFill("solid", fgColor=h)
thin = Side(style="thin", color="BFBFBF"); box = Border(left=thin, right=thin, top=thin, bottom=thin)
F9 = Font(size=9); F9B = Font(size=9, bold=True)

# small icon copies keep the workbook light
os.makedirs(os.path.join(HERE, "icons_small"), exist_ok=True)
def icon(part):
    src = os.path.join(HERE, "icons", part + ".png"); dst = os.path.join(HERE, "icons_small", part + ".png")
    if not os.path.exists(dst):
        PILImage.open(src).convert("RGBA").resize((156, 156), PILImage.LANCZOS).save(dst)
    im = Image(dst); im.width = im.height = 52
    return im

def joint(code):
    s = [x for x in code.replace("?", "").split("+") if x]
    return min(s, key=PRI.index) if s else None

wb = Workbook(); ws = wb.active; ws.title = "Compatibility"
n = len(order); R0, C0 = 8, 6                       # first matrix cell = F8
ws["A1"] = "LEGO part-subtype compatibility matrix (theoretical, from lego_theoretical_compatibility.R)"
ws["A1"].font = Font(size=13, bold=True)
ws["A2"] = ("Cell = joint system the two subtypes can form (shading = joint colour, see Legend); '?' = medium certainty. "
            "BLANK = cannot join directly (a proximity contact between them is a false positive). "
            "Each subtype is shown by one illustrative part (icon, part number, LDraw name).")
ws["A2"].font = Font(size=9, italic=True)
for r, lab in ((3, "Icon"), (4, "Part"), (5, "Part name"), (6, "Subtype"), (7, "Category / Part →")):
    c = ws.cell(r, 5, lab); c.font = F9B; c.alignment = Alignment(horizontal="right", vertical="center")
for c, h in zip(range(1, 6), ("Icon", "Part", "Part name", "Subtype", "Category")):
    x = ws.cell(7, c, h); x.font = F9B; x.fill = fill("EEEEEE"); x.alignment = Alignment(vertical="center")
ws.cell(7, 5, "Category / Part →")
for i, st in enumerate(order):
    part = REPS[st]; cat = sub[st]["category"]; col = C0 + i; row = R0 + i; cf = fill(CAT[cat])
    nm = desc.get(part, "")
    for r, v, rot in ((4, part, 0), (5, nm, 90), (6, st, 90), (7, cat, 0)):
        c = ws.cell(r, col, v); c.font = F9B if r == 4 else F9; c.fill = cf; c.border = box
        c.alignment = Alignment(horizontal="center", vertical="bottom" if rot else "center", textRotation=rot, wrap_text=bool(rot))
    ws.add_image(icon(part), f"{L(col)}3")
    for c, v in ((2, part), (3, nm), (4, st), (5, cat)):
        x = ws.cell(row, c, v); x.font = F9B if c == 2 else F9; x.fill = cf; x.border = box
        x.alignment = Alignment(vertical="center", horizontal="center" if c in (2, 5) else None, wrap_text=(c == 4))
    ws.add_image(icon(part), f"A{row}")
    for j, sb in enumerate(order):
        code = cell[st][sb]; c = ws.cell(row, C0 + j, code or None); c.border = box
        c.font = Font(size=8, italic=code.endswith("?")); c.alignment = Alignment(horizontal="center", vertical="center")
        if code: c.fill = fill(JOINT[joint(code)])
ws.row_dimensions[3].height = 42; ws.row_dimensions[4].height = 15
ws.row_dimensions[5].height = 175; ws.row_dimensions[6].height = 150; ws.row_dimensions[7].height = 22
for r in range(R0, R0 + n): ws.row_dimensions[r].height = 42
for c, w in zip("ABCDE", (8.5, 9, 34, 30, 17)): ws.column_dimensions[c].width = w
for j in range(n): ws.column_dimensions[L(C0 + j)].width = 8.5
ws.freeze_panes = ws.cell(R0, C0)

# ---- Subtypes sheet
s2 = wb.create_sheet("Subtypes")
hdr = ["Icon", "Part", "Part name", "Category", "Subtype", "Family", "Interfaces", "Examples (project table)", "# compatible subtypes"]
for c, h in enumerate(hdr, 1):
    x = s2.cell(1, c, h); x.font = F9B; x.fill = fill("DDDDDD"); x.border = box
for i, st in enumerate(order):
    r = i + 2; p = REPS[st]; d = sub[st]; k = sum(1 for sb in order if cell[st][sb])
    for c, v in enumerate(["", p, desc.get(p, ""), d["category"], st, d["family"], d["interfaces"], d["examples"], k], 1):
        x = s2.cell(r, c, v or None); x.font = F9; x.border = box; x.alignment = Alignment(vertical="center", wrap_text=c in (3, 5, 8))
        if c in (4, 5): x.fill = fill(CAT[d["category"]])
    s2.row_dimensions[r].height = 42; s2.add_image(icon(p), f"A{r}")
for c, w in zip("ABCDEFGHI", (8.5, 9, 40, 11, 34, 13, 14, 38, 12)): s2.column_dimensions[c].width = w
s2.freeze_panes = "B2"

# ---- Legend sheet
s3 = wb.create_sheet("Legend")
s3["A1"] = "Joint colours"; s3["A1"].font = Font(size=11, bold=True)
for i, j in enumerate(PRI):
    s3.cell(i + 2, 1, j).fill = fill(JOINT[j]); s3.cell(i + 2, 1).font = F9; s3.cell(i + 2, 1).border = box
r = len(PRI) + 3
s3.cell(r, 1, "Blank cell = cannot join directly; '?' = medium certainty; A+B = several joints, coloured by the first in priority order").font = Font(size=9, italic=True)
r += 2
for title, f, cols in (("Interfaces", "interfaces.csv", ("code", "interface", "description")),
                       ("Mating rules", "mating_rules.csv", ("a", "b", "system", "certainty", "note"))):
    p = os.path.join(HERE, f)
    if not os.path.exists(p): continue
    s3.cell(r, 1, title).font = Font(size=11, bold=True); r += 1
    for c, h in enumerate(cols, 1): s3.cell(r, c, h).font = F9B
    r += 1
    for row in csv.DictReader(open(p, encoding="utf-8")):
        for c, h in enumerate(cols, 1): s3.cell(r, c, row[h]).font = F9
        r += 1
    r += 1
for c, w in zip("ABCDE", (14, 14, 40, 12, 50)): s3.column_dimensions[c].width = w

out = os.path.join(HERE, "compat_matrix_subtype_icons.xlsx"); wb.save(out)
print("saved", out, "| subtypes", n, "| compatible cells", sum(1 for a in order for b in order if cell[a][b]))
