# =============================================================================
# mpd_parts_table.R
#
# Extract LEGO parts from an LDraw .mpd file:
#   - part filename (e.g. 3004.dat)
#   - human part name (e.g. "Brick  1 x  2")
#   - part category  (e.g. "Brick")
#   - part count in the model
#   - local image path
#
# Outputs:
#   1. parts_df        — R dataframe (in environment)
#   2. <set>_parts.csv — tidy CSV
#   3. <set>_parts.html — standalone illustrated HTML table
#
# All outputs saved to OUT_DIR.
# =============================================================================

library(tidyverse)

# ── Paths ────────────────────────────────────────────────────────────────────

MPD_FILE   <- "/Users/pedro/Documents/LEGO Creations/Utils/ldraw/311-1-Ferry.mpd"
PARTS_LST  <- "/Users/pedro/Documents/LEGO Creations/Utils/ldraw/Parts.lst"
IMAGES_DIR <- "/Users/pedro/Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/images/alphin"
OUT_DIR    <- "/Users/pedro/Documents/Working/~RCode/MyRCode/LEGO/LEGO_sets_networks/code/LEGO_networks_cl/files"

# Derive output file stems from the MPD filename
set_stem   <- tools::file_path_sans_ext(basename(MPD_FILE))   # "311-1-Ferry"
OUT_CSV    <- file.path(OUT_DIR, paste0(set_stem, "_parts.csv"))
OUT_HTML   <- file.path(OUT_DIR, paste0(set_stem, "_parts.html"))


# ── 1. Load Parts.lst ────────────────────────────────────────────────────────
# Format per line:  "3004.dat       =Brick  1 x  2   "
# Split on first "=": left = filename, right = human name.
# Category = first word of the human name.

parts_lst <- read_lines(PARTS_LST) |>
  tibble(raw = _) |>
  filter(str_detect(raw, "=")) |>
  mutate(
    part_file_lower = str_to_lower(str_trim(str_extract(raw, "^[^=]+"))),
    part_name       = str_squish(str_extract(raw, "(?<==).*"))
  ) |>
  mutate(category = word(part_name, 1)) |>
  select(part_file_lower, part_name, category)

message(sprintf("Parts.lst: %d entries", nrow(parts_lst)))


# ── 2. Build image lookup ────────────────────────────────────────────────────
# Images are named by part number only, e.g. 3004.png, 3062a.png.
# Strategy:
#   Pass 1 – exact match: "3062a.dat" -> key "3062a" -> "3062a.png"
#   Pass 2 – strip trailing letters: "3062a" -> "3062" -> "3062.png"

img_lookup <- tibble(
  img_filename = list.files(IMAGES_DIR, pattern = "\\.png$", ignore.case = TRUE)
) |>
  mutate(
    img_key  = str_to_lower(tools::file_path_sans_ext(img_filename)),
    img_path = file.path(IMAGES_DIR, img_filename)
  )

message(sprintf("Images available: %d", nrow(img_lookup)))


# ── 3. Parse the MPD ─────────────────────────────────────────────────────────

parse_mpd <- function(path) {
  lines <- read_lines(path)

  # 3a. Embedded descriptions for custom/unofficial parts --------------------
  # Block header:  0 FILE <filename>
  # Description :  0 <human name>   (next non-blank line, not a meta-command)
  file_idx <- which(str_detect(lines, "^0\\s+FILE\\s+"))
  embedded <- map_dfr(file_idx, function(i) {
    fname     <- str_trim(str_remove(lines[i], "^0\\s+FILE\\s+"))
    desc_line <- lines[i + 1]
    pname     <- if (str_detect(desc_line, "^0\\s+[^!~]"))
                   str_squish(str_remove(desc_line, "^0\\s+"))
                 else NA_character_
    tibble(part_file = fname, part_name_emb = pname)
  })

  # 3b. All part references with instance counts ----------------------------
  # Type-1 line: 1 <col> <x> <y> <z> <3×3 matrix> <partfile>
  # Part filename = last whitespace-separated token.
  counts <- lines |>
    str_subset("^1\\s+") |>
    str_trim() |>
    str_extract("[^\\s]+$") |>
    tibble(part_file = _) |>
    count(part_file, name = "count")

  list(embedded = embedded, counts = counts)
}

mpd <- parse_mpd(MPD_FILE)
message(sprintf("Unique part refs: %d  |  total instances: %d",
                nrow(mpd$counts), sum(mpd$counts$count)))


# ── 4. Resolve names, categories, images ─────────────────────────────────────

parts_df <- mpd$counts |>
  mutate(part_file_lower = str_to_lower(part_file)) |>

  # Name/category from Parts.lst (primary)
  left_join(parts_lst, by = "part_file_lower") |>
  mutate(source = if_else(!is.na(part_name), "parts_lst", NA_character_)) |>

  # Name from MPD-embedded descriptions (fallback for custom parts)
  left_join(
    mpd$embedded |>
      mutate(part_file_lower = str_to_lower(part_file)) |>
      select(part_file_lower, part_name_emb),
    by = "part_file_lower"
  ) |>
  mutate(
    part_name = coalesce(part_name, part_name_emb),
    category  = if_else(is.na(source) & !is.na(part_name_emb),
                        "Custom/Unofficial", category),
    source    = case_when(
      !is.na(source)        ~ source,
      !is.na(part_name_emb) ~ "mpd_embedded",
      TRUE                  ~ "not_found"
    )
  ) |>

  # Derive lookup key: basename without .dat/.ldr, lowercased
  mutate(
    part_key = str_to_lower(
      tools::file_path_sans_ext(basename(part_file_lower))
    ),
    part_key_base = str_remove(part_key, "[a-z]+$")   # strip trailing letters
  ) |>

  # Pass 1: exact key match
  left_join(img_lookup |> select(img_key, img_path),
            by = c("part_key" = "img_key")) |>

  # Pass 2: base key match when exact failed
  left_join(img_lookup |> select(img_key, img_path) |> rename(img_path2 = img_path),
            by = c("part_key_base" = "img_key")) |>
  mutate(img_path = coalesce(img_path, img_path2)) |>

  select(part_file, part_name, category, count, img_path, source) |>
  arrange(category, part_name)

# ── Summary ──────────────────────────────────────────────────────────────────
message("\n── Source breakdown ──────────────────────────────")
parts_df |> count(source) |> print()

message("\n── Category breakdown (resolved parts) ──────────")
parts_df |>
  filter(source != "not_found") |>
  count(category, sort = TRUE) |>
  print(n = 30)

n_real   <- sum(parts_df$source != "not_found")
n_pieces <- parts_df |> filter(source != "not_found") |> pull(count) |> sum()
n_img    <- parts_df |> filter(source != "not_found", !is.na(img_path)) |> nrow()

message(sprintf("\nReal parts  : %d unique  |  %d total pieces", n_real, n_pieces))
message(sprintf("Images found: %d / %d", n_img, n_real))


# ── 5. Save CSV ───────────────────────────────────────────────────────────────

parts_df |>
  select(part_file, part_name, category, count, img_path) |>
  write_csv(OUT_CSV)
message(sprintf("CSV saved: %s", OUT_CSV))


# ── 6. Build HTML table ───────────────────────────────────────────────────────

# Work only with real parts (drop bare LDraw primitives)
table_df <- parts_df |> filter(source != "not_found")

# HTML-escape helper (avoids htmltools dependency)
he <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x |>
    str_replace_all("&",  "&amp;")  |>
    str_replace_all("<",  "&lt;")   |>
    str_replace_all(">",  "&gt;")   |>
    str_replace_all('"',  "&quot;")
}

# Build one <tr> per part
make_row <- function(part_file, part_name, category, count, img_path, source) {
  img_tag <- if (!is.na(img_path)) {
    sprintf('<img src="file://%s" alt="%s" loading="lazy">',
            img_path, he(part_file))
  } else {
    '<div class="no-img">?</div>'
  }

  name_cell <- if (!is.na(part_name) && nchar(part_name) > 0)
    he(part_name) else '<span class="na">—</span>'

  cat_cell  <- if (!is.na(category) && nchar(category) > 0)
    sprintf('<span class="badge">%s</span>', he(category))
  else '<span class="na">—</span>'

  sprintf(
    '<tr>\n  <td class="c-img">%s</td>\n  <td class="c-cat">%s</td>\n  <td class="c-name">%s</td>\n  <td class="c-file"><code>%s</code></td>\n  <td class="c-count"><b>%d</b></td>\n</tr>',
    img_tag, cat_cell, name_cell, he(part_file), count
  )
}

rows_html <- table_df |>
  pmap_chr(make_row) |>
  paste(collapse = "\n")

# Category <option> list for filter dropdown
cat_options <- table_df |>
  filter(!is.na(category)) |>
  distinct(category) |>
  arrange(category) |>
  pull(category) |>
  map_chr(\(c) sprintf('<option value="%s">%s</option>', he(tolower(c)), he(c))) |>
  paste(collapse = "\n      ")

html_out <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>%s — Parts</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=Barlow:wght@400;600;800&display=swap" rel="stylesheet">
<style>
/* ── Reset & tokens ───────────────────────────────────────────────────── */
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
:root{
  --bg:#12131a; --surf:#1c1e28; --surf2:#23263a;
  --border:#2e3248; --accent:#f7c948; --accent2:#ff5533;
  --text:#dde1f0; --muted:#6a6f8e; --radius:5px;
  --font-ui:"Barlow",sans-serif; --font-mono:"Space Mono",monospace;
}
body{background:var(--bg);color:var(--text);font-family:var(--font-ui);font-size:14px;min-height:100vh}

/* ── Header ──────────────────────────────────────────────────────────── */
header{
  background:var(--surf);
  border-bottom:3px solid var(--accent);
  padding:22px 36px 18px;
  display:flex; align-items:flex-end; gap:28px; flex-wrap:wrap;
}
.h-title{font-size:1.9rem;font-weight:800;letter-spacing:-.02em;color:var(--accent);line-height:1}
.h-sub{font-size:.7rem;letter-spacing:.14em;text-transform:uppercase;color:var(--muted);margin-bottom:4px}
.stats{display:flex;gap:22px;margin-left:auto}
.stat-val{font-size:1.5rem;font-weight:800;color:var(--accent);font-variant-numeric:tabular-nums;line-height:1}
.stat-lbl{font-size:.65rem;letter-spacing:.1em;text-transform:uppercase;color:var(--muted)}

/* ── Toolbar ─────────────────────────────────────────────────────────── */
.toolbar{
  position:sticky;top:0;z-index:20;
  background:var(--bg);border-bottom:1px solid var(--border);
  padding:10px 36px; display:flex; align-items:center; gap:12px; flex-wrap:wrap;
}
.toolbar label{font-size:.7rem;letter-spacing:.1em;text-transform:uppercase;color:var(--muted)}
.toolbar input,.toolbar select{
  background:var(--surf2);border:1px solid var(--border);border-radius:var(--radius);
  color:var(--text);font-family:var(--font-ui);font-size:.85rem;
  padding:5px 11px;outline:none;transition:border-color .15s;
}
.toolbar input{width:200px}
.toolbar input:focus,.toolbar select:focus{border-color:var(--accent)}
#hit-count{margin-left:auto;font-size:.72rem;font-family:var(--font-mono);color:var(--muted)}

/* ── Table ───────────────────────────────────────────────────────────── */
.wrap{padding:20px 36px 60px;overflow-x:auto}
table{width:100%%;border-collapse:collapse}
thead th{
  background:var(--surf);border-bottom:2px solid var(--border);
  padding:9px 14px;text-align:left;
  font-size:.68rem;letter-spacing:.13em;text-transform:uppercase;color:var(--muted);
  cursor:pointer;user-select:none;white-space:nowrap;
}
thead th:hover{color:var(--accent)}
thead th.asc::after{content:" ▲";color:var(--accent)}
thead th.desc::after{content:" ▼";color:var(--accent)}
thead th:first-child{cursor:default}  /* image col not sortable */

tbody tr{border-bottom:1px solid var(--border);transition:background .1s}
tbody tr:hover{background:var(--surf)}
td{padding:7px 14px;vertical-align:middle}

/* Image cell */
.c-img{width:72px;text-align:center}
.c-img img{
  width:58px;height:58px;object-fit:contain;display:block;margin:0 auto;
  background:#1e2030;border-radius:4px;padding:3px;
}
.no-img{
  width:58px;height:58px;border-radius:4px;background:var(--surf2);
  border:1px dashed var(--border);display:flex;align-items:center;
  justify-content:center;color:var(--muted);font-size:1.1rem;margin:0 auto;
}
/* Category badge */
.badge{
  display:inline-block;background:var(--surf2);border:1px solid var(--border);
  border-radius:3px;padding:2px 7px;
  font-size:.7rem;letter-spacing:.05em;color:var(--accent);white-space:nowrap;
}
/* Count */
.c-count{text-align:center}
.c-count b{
  display:inline-block;background:var(--accent2);color:#fff;
  border-radius:4px;padding:1px 9px;font-size:.88rem;
  font-family:var(--font-mono);min-width:28px;text-align:center;
}
.c-name{font-weight:600}
.c-file code{font-family:var(--font-mono);font-size:.76rem;color:var(--muted)}
.na{color:var(--muted)}
.hidden{display:none!important}
</style>
</head>
<body>

<header>
  <div>
    <div class="h-sub">LDraw model</div>
    <div class="h-title">%s</div>
  </div>
  <div class="stats">
    <div class="stat"><div class="stat-val">%d</div><div class="stat-lbl">Unique parts</div></div>
    <div class="stat"><div class="stat-val">%d</div><div class="stat-lbl">Total pieces</div></div>
    <div class="stat"><div class="stat-val">%d</div><div class="stat-lbl">With image</div></div>
  </div>
</header>

<div class="toolbar">
  <label for="q">Search</label>
  <input id="q" type="text" placeholder="name or filename…">
  <label for="cat">Category</label>
  <select id="cat">
    <option value="">All</option>
    %s
  </select>
  <span id="hit-count"></span>
</div>

<div class="wrap">
<table id="t">
  <thead>
    <tr>
      <th>Image</th>
      <th data-col="1">Category</th>
      <th data-col="2">Part name</th>
      <th data-col="3">Filename</th>
      <th data-col="4">Count</th>
    </tr>
  </thead>
  <tbody>
%s
  </tbody>
</table>
</div>

<script>
const q   = document.getElementById("q");
const cat = document.getElementById("cat");
const hc  = document.getElementById("hit-count");
const tb  = document.querySelector("#t tbody");

function filter() {
  const sq = q.value.toLowerCase();
  const sc = cat.value.toLowerCase();
  let n = 0;
  for (const tr of tb.rows) {
    const name = tr.cells[2].textContent.toLowerCase();
    const file = tr.cells[3].textContent.toLowerCase();
    const rc   = (tr.cells[1].querySelector(".badge")?.textContent || "").toLowerCase();
    const ok   = (!sq || name.includes(sq) || file.includes(sq)) && (!sc || rc === sc);
    tr.classList.toggle("hidden", !ok);
    if (ok) n++;
  }
  hc.textContent = n + " shown";
}
q.addEventListener("input", filter);
cat.addEventListener("change", filter);
filter();

// ── Column sort
let sortCol = -1, asc = true;
document.querySelectorAll("thead th[data-col]").forEach(th => {
  th.addEventListener("click", () => {
    const col = +th.dataset.col;
    asc = (sortCol === col) ? !asc : true;
    sortCol = col;
    document.querySelectorAll("thead th").forEach(h => h.classList.remove("asc","desc"));
    th.classList.add(asc ? "asc" : "desc");
    const rows = [...tb.rows].sort((a,b) => {
      const av = a.cells[col].textContent.trim();
      const bv = b.cells[col].textContent.trim();
      if (col === 4) return asc ? +av - +bv : +bv - +av;
      return asc ? av.localeCompare(bv) : bv.localeCompare(av);
    });
    rows.forEach(r => tb.appendChild(r));
  });
});
</script>
</body>
</html>',
  # <title>
  he(set_stem),
  # header title
  he(set_stem),
  # stats
  n_real, n_pieces, n_img,
  # category options
  cat_options,
  # table rows
  rows_html
)

write_lines(html_out, OUT_HTML)
message(sprintf("HTML saved : %s", OUT_HTML))
message("Done.")
