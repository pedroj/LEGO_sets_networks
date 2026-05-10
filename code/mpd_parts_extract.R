# =============================================================================
# Extract LEGO parts, human names, and categories from an LDraw .mpd file
# Uses the local LDraw Parts.lst for name and category lookup.
#
# Output dataframe columns:
#   part_file  : LDraw filename, e.g. "3004.dat"
#   part_name  : Human-readable name, e.g. "Brick  1 x  2"
#   category   : First word of the name, e.g. "Brick"
#   source     : "parts_lst" | "mpd_embedded" | "not_found"
# =============================================================================

library(tidyverse)

# --- Configuration -----------------------------------------------------------

MPD_FILE   <- "/Users/pedro/Documents/LEGO Creations/Utils/ldraw/311-1-Ferry.mpd"
PARTS_LST  <- "/Users/pedro/Documents/LEGO Creations/Utils/ldraw/Parts.lst"


# --- 1. Load Parts.lst -------------------------------------------------------
# Format per line:  "3004.dat       =Brick  1 x  2   "
# Split on the first "=" to get filename and name.

parts_lst <- read_lines(PARTS_LST) |>
  tibble(raw = _) |>
  filter(str_detect(raw, "=")) |>                        # skip blank/header lines
  mutate(
    part_file_lower = str_trim(str_extract(raw, "^[^=]+")),
    part_name       = str_squish(str_extract(raw, "(?<=\\=).*"))
  ) |>
  mutate(
    part_file_lower = str_to_lower(part_file_lower),
    category        = word(part_name, 1)                 # first word = category
  ) |>
  select(part_file_lower, part_name, category)

cat(sprintf("Parts.lst loaded: %d entries.\n", nrow(parts_lst)))


# --- 2. Parse the MPD file ---------------------------------------------------

parse_mpd <- function(mpd_path) {
  lines <- read_lines(mpd_path)

  # ---- 2a. Embedded part descriptions (custom/unofficial parts) ------------
  # Blocks start with:  0 FILE <filename>
  # Followed by:        0 <Human Part Name>   (not a meta-command starting with !)

  file_idx <- which(str_detect(lines, "^0\\s+FILE\\s+"))

  embedded <- map_dfr(file_idx, function(i) {
    filename  <- str_trim(str_remove(lines[i], "^0\\s+FILE\\s+"))
    desc_line <- lines[i + 1]
    part_name <- if (str_detect(desc_line, "^0\\s+[^!~]")) {
      str_squish(str_remove(desc_line, "^0\\s+"))
    } else {
      NA_character_
    }
    tibble(part_file = filename, part_name = part_name)
  })

  # ---- 2b. All part references (type-1 lines) ------------------------------
  # Format: 1 <colour> <x> <y> <z> <3x3 matrix> <partfile>
  # The part filename is always the last token.

  referenced <- lines |>
    str_subset("^1\\s+") |>
    str_trim() |>
    str_extract("[^\\s]+$") |>                           # last token
    unique() |>
    tibble(part_file = _)

  list(embedded = embedded, referenced = referenced)
}

mpd <- parse_mpd(MPD_FILE)

cat(sprintf("Unique part references in MPD : %d\n", nrow(mpd$referenced)))
cat(sprintf("Embedded descriptions in MPD  : %d\n", nrow(mpd$embedded)))


# --- 3. Classify each referenced part ----------------------------------------
# Priority: (1) Parts.lst lookup, (2) MPD-embedded description, (3) not_found
# Parts.lst covers all standard LDraw parts; embedded covers custom/unofficial.
# LDraw primitives (geometry helpers) are referenced too but won't appear in
# Parts.lst — they end up as "not_found", which is expected and informative.

parts_df <- mpd$referenced |>
  mutate(part_file_lower = str_to_lower(part_file)) |>

  # Join Parts.lst
  left_join(parts_lst, by = "part_file_lower") |>
  mutate(source = if_else(!is.na(part_name), "parts_lst", NA_character_)) |>

  # For anything not in Parts.lst, try the MPD-embedded descriptions
  left_join(
    mpd$embedded |>
      transmute(part_file_lower = str_to_lower(part_file),
                part_name_emb   = part_name),
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
  select(part_file, part_name, category, source) |>
  arrange(category, part_name)


# --- 4. Summary --------------------------------------------------------------

cat("\n--- Source breakdown ---\n")
parts_df |> count(source) |> print()

cat("\n--- Category breakdown (resolved parts only) ---\n")
parts_df |>
  filter(source != "not_found") |>
  count(category, sort = TRUE) |>
  print(n = 30)

cat(sprintf("\nTotal unique parts  : %d\n", nrow(parts_df)))
cat(sprintf("Resolved            : %d\n", sum(parts_df$source != "not_found")))
cat(sprintf("Not found           : %d\n", sum(parts_df$source == "not_found")))


# --- 5. Preview & save -------------------------------------------------------

cat("\n--- Preview (resolved parts) ---\n")
parts_df |> filter(source != "not_found") |> print(n = 20)

out_path <- str_replace(MPD_FILE, "\\.mpd$", "_parts.csv")
write_csv(parts_df, out_path)
cat(sprintf("\nSaved to: %s\n", out_path))
