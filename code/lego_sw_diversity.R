# ============================================================
# LEGO Star Wars – Set Size vs. Parts Diversity Analysis
# Data: local Rebrickable CSVs + API for theme tree validation
# Output: standalone PNG files in the same extdata folder
# ============================================================

# ── 0. Dependencies ──────────────────────────────────────────
library(tidyverse)
library(ggrepel)
library(scales)
library(corrplot)
library(mgcv)        # GAM smoothing
library(httr2)       # Rebrickable API calls

# ── 1. Paths & API key ───────────────────────────────────────
data_dir <- "/Users/pedro/Documents/Working/~RCode/MyRCode/LEGO/LEGO_Parts/inst/extdata"
api_key  <- Sys.getenv("REBRICKABLE_API_KEY")
if (nchar(api_key) == 0) stop("REBRICKABLE_API_KEY not found in environment.")

# ── 2. Load local CSVs ───────────────────────────────────────
message("Loading local CSVs...")
sets            <- read_csv(file.path(data_dir, "sets.csv"),            show_col_types = FALSE)
inventories     <- read_csv(file.path(data_dir, "inventories.csv"),     show_col_types = FALSE)
inventory_parts <- read_csv(file.path(data_dir, "inventory_parts.csv"), show_col_types = FALSE)
themes          <- read_csv(file.path(data_dir, "themes.csv"),          show_col_types = FALSE)
parts           <- read_csv(file.path(data_dir, "parts.csv"),           show_col_types = FALSE)
part_categories <- read_csv(file.path(data_dir, "part_categories.csv"), show_col_types = FALSE)

# ── 3. Resolve full Star Wars theme tree ─────────────────────
# The themes CSV has two "Star Wars" roots:
#   id=18  (child of Technic id=1) – legacy / early sets
#   id=158 (top-level)             – main modern tree
# We walk the full parent→child tree for both roots, then also
# fetch from the API to catch any themes added after CSV export.

sw_root_ids <- c(18L, 158L)

# Recursive tree-walk on the local CSV
get_all_child_ids <- function(parent_ids, theme_tbl) {
  all_ids <- parent_ids
  repeat {
    children <- theme_tbl %>%
      filter(parent_id %in% all_ids, !id %in% all_ids) %>%
      pull(id)
    if (length(children) == 0) break
    all_ids <- c(all_ids, children)
  }
  all_ids
}

sw_theme_ids_local <- get_all_child_ids(sw_root_ids, themes)
message(sprintf("Star Wars theme IDs from local CSV: %d themes", length(sw_theme_ids_local)))

# ── 4. Validate / extend via Rebrickable API ─────────────────
# Fetch all themes from the API in paginated calls and add any
# that are present in the API but missing from the local CSV.
fetch_api_themes <- function(api_key) {
  url      <- "https://rebrickable.com/api/v3/lego/themes/"
  all_rows <- list()
  page     <- 1L
  repeat {
    resp <- request(url) %>%
      req_headers(Authorization = paste("key", api_key)) %>%
      req_url_query(page = page, page_size = 1000L) %>%
      req_perform()
    body <- resp_body_json(resp, simplifyVector = TRUE)
    all_rows <- c(all_rows, list(as_tibble(body$results)))
    if (is.null(body$`next`)) break
    page <- page + 1L
  }
  bind_rows(all_rows)
}

message("Fetching theme list from Rebrickable API...")
api_themes <- fetch_api_themes(api_key)

# Merge API themes with local, keeping all
themes_full <- bind_rows(
  themes,
  api_themes %>%
    rename(parent_id = parent_id) %>%
    anti_join(themes, by = "id")
)

# Re-resolve SW tree with the enriched table
sw_theme_ids <- get_all_child_ids(sw_root_ids, themes_full)
message(sprintf("Star Wars theme IDs after API merge: %d themes", length(sw_theme_ids)))

# ── 5. Filter to Star Wars sets & build parts table ──────────
message("Filtering sets and joining parts...")

sw_sets <- sets %>%
  filter(theme_id %in% sw_theme_ids)

message(sprintf("  Star Wars sets found: %d", nrow(sw_sets)))

# Keep only the latest inventory version per set
latest_inv <- inventories %>%
  group_by(set_num) %>%
  slice_max(version, n = 1, with_ties = FALSE) %>%
  ungroup()

# Join: inventory_parts → inventories → sw_sets
# Also bring in part category via parts table
set_parts <- latest_inv %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  # is_spare arrives as "t"/"f" or logical depending on CSV version
  mutate(is_spare = is_spare %in% c(TRUE, "t", "T", "true", "True")) %>%
  filter(!is_spare) %>%
  inner_join(sw_sets, by = "set_num") %>%
  left_join(parts %>% select(part_num, part_cat_id), by = "part_num") %>%
  left_join(part_categories %>% select(id, cat_name = name),
            by = c("part_cat_id" = "id")) %>%
  # Attach sub-theme name
  left_join(themes_full %>% select(id, subtheme = name),
            by = c("theme_id" = "id"))

# ── 6. Compute per-set diversity metrics ─────────────────────
message("Computing diversity metrics...")

sw_diversity <- set_parts %>%
  group_by(set_num, name, year, theme_id, subtheme, num_parts) %>%
  summarise(
    total_parts     = sum(quantity),
    n_unique_parts  = n_distinct(part_num),
    n_unique_colors = n_distinct(color_id),
    n_unique_cats   = n_distinct(cat_name, na.rm = TRUE),

    # Shannon entropy over part-type abundances
    shannon_parts   = {
      q  <- quantity / sum(quantity)
      -sum(q * log(q + 1e-10))
    },

    # Simpson's diversity  (1 - Σpᵢ²)
    simpson         = {
      p <- quantity / sum(quantity)
      1 - sum(p^2)
    },

    # Pielou's evenness J = H / ln(S)
    evenness        = if_else(
      n_distinct(part_num) > 1,
      (-sum((quantity / sum(quantity)) * log(quantity / sum(quantity) + 1e-10))) /
        log(n_distinct(part_num)),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  filter(total_parts >= 5)   # drop near-empty promotional entries

message(sprintf("  Sets retained after filtering (≥5 parts): %d", nrow(sw_diversity)))

# ── 7. Correlation analysis ───────────────────────────────────
message("\n── Spearman correlation matrix ──")
cor_vars <- sw_diversity %>%
  select(total_parts, n_unique_parts, n_unique_colors, n_unique_cats,
         shannon_parts, simpson, evenness) %>%
  drop_na()

print(round(cor(cor_vars, method = "spearman"), 3))

ct <- cor.test(sw_diversity$total_parts, sw_diversity$shannon_parts,
               method = "spearman")
message(sprintf(
  "\nSpearman rho (total_parts ~ shannon_parts): %.3f   p = %.2e   n = %d",
  ct$estimate, ct$p.value, nrow(sw_diversity)
))

# ── 8. Shared ggplot theme ────────────────────────────────────
sw_yellow <- "#FFD700"
sw_blue   <- "#1E90FF"
sw_red    <- "#B22222"
sw_bg     <- "#0d1117"   # near-black space background
sw_text   <- "#E8E8E8"

theme_sw <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = sw_bg, colour = NA),
    panel.background = element_rect(fill = sw_bg, colour = NA),
    panel.grid.major = element_line(colour = "#2a2a3a"),
    panel.grid.minor = element_blank(),
    text             = element_text(colour = sw_text),
    axis.text        = element_text(colour = sw_text),
    plot.title       = element_text(face = "bold", size = 15,
                                    colour = sw_yellow),
    plot.subtitle    = element_text(colour = "#aaaaaa", size = 11),
    plot.caption     = element_text(colour = "#666666", size = 9),
    legend.background = element_rect(fill = sw_bg, colour = NA),
    legend.text      = element_text(colour = sw_text),
    legend.title     = element_text(colour = sw_text)
  )

save_png <- function(plot, filename, w = 10, h = 7) {
  ggsave(file.path(data_dir, filename), plot,
         width = w, height = h, dpi = 150, bg = sw_bg)
  message(sprintf("  Saved: %s", filename))
}

# ── 9. Plot 1 – Scatter: total parts vs. Shannon diversity ────
message("Building plots...")

# Label the 10 largest sets for context
top_sets <- sw_diversity %>% slice_max(total_parts, n = 10)

p1 <- sw_diversity %>%
  ggplot(aes(x = total_parts, y = shannon_parts)) +
  geom_point(aes(colour = year), alpha = 0.55, size = 2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              colour = sw_yellow, linewidth = 1.1, se = TRUE,
              fill = sw_yellow, alpha = 0.15) +
  geom_text_repel(data = top_sets,
                  aes(label = str_wrap(name, 25)),
                  colour = sw_text, size = 2.8,
                  max.overlaps = 15, segment.colour = "#555555") +
  scale_x_log10(labels = comma) +
  scale_colour_gradient(low = sw_blue, high = sw_red,
                        name = "Year") +
  labs(
    title    = "LEGO Star Wars: Set Size vs. Parts Diversity",
    subtitle = "Each point = one set  |  GAM smoother (log₁₀ x-axis)  |  10 largest sets labelled",
    x        = "Total parts (log₁₀ scale)",
    y        = "Shannon diversity H′ (part types)",
    caption  = "Source: Rebrickable"
  ) +
  theme_sw

save_png(p1, "sw_p1_scatter_size_diversity.png")

# ── 10. Plot 2 – Spearman correlation heatmap ─────────────────
png(file.path(data_dir, "sw_p2_correlation_heatmap.png"),
    width = 800, height = 750, bg = sw_bg)
par(bg = sw_bg, col.main = sw_yellow, col.lab = sw_text,
    col.axis = sw_text, fg = sw_text)
corrplot(
  cor(cor_vars, method = "spearman"),
  method       = "color",
  type         = "upper",
  addCoef.col  = sw_text,
  number.cex   = 0.85,
  tl.col       = sw_text,
  tl.srt       = 30,
  col          = colorRampPalette(c(sw_red, sw_bg, sw_blue))(200),
  title        = "Spearman Correlations – Star Wars Diversity Metrics",
  mar          = c(0, 0, 2, 0)
)
dev.off()
message("  Saved: sw_p2_correlation_heatmap.png")

# ── 11. Plot 3 – Diversity by decade × parts-count decile ─────
p3 <- sw_diversity %>%
  mutate(
    decade       = factor((year %/% 10) * 10),
    parts_decile = ntile(total_parts, 10)
  ) %>%
  group_by(decade, parts_decile) %>%
  summarise(mean_shannon = mean(shannon_parts, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = parts_decile, y = mean_shannon,
             colour = decade, group = decade)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(
    values = colorRampPalette(c(sw_blue, sw_yellow, sw_red))(
      n_distinct(sw_diversity$year %/% 10)
    ),
    name = "Decade"
  ) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title    = "Parts Diversity by Set-Size Decile Across Decades",
    subtitle = "Decile 1 = smallest sets, 10 = largest  |  Star Wars only",
    x        = "Parts-count decile",
    y        = "Mean Shannon diversity H′",
    caption  = "Source: Rebrickable"
  ) +
  theme_sw

save_png(p3, "sw_p3_decade_decile_diversity.png")

# ── 12. Plot 4 – Sub-theme OLS slope (size → diversity) ───────
subtheme_slopes <- sw_diversity %>%
  group_by(subtheme) %>%
  filter(n() >= 10) %>%   # need enough sets for a stable regression
  summarise(
    beta = coef(lm(shannon_parts ~ log1p(total_parts)))[2],
    r2   = summary(lm(shannon_parts ~ log1p(total_parts)))$r.squared,
    n    = n(),
    .groups = "drop"
  ) %>%
  slice_max(abs(beta), n = 20)

p4 <- subtheme_slopes %>%
  mutate(subtheme = fct_reorder(subtheme, beta)) %>%
  ggplot(aes(x = beta, y = subtheme, fill = beta > 0)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = sprintf("n=%d", n),
                x = if_else(beta > 0, -0.01, 0.01)),
            hjust = if_else(subtheme_slopes$beta > 0, 1, 0),
            colour = sw_text, size = 3) +
  scale_fill_manual(values = c("TRUE" = sw_blue, "FALSE" = sw_red)) +
  labs(
    title    = "Which Star Wars sub-themes show strongest size–diversity covariation?",
    subtitle = "OLS slope β: Shannon H′ ~ log(total_parts)  |  sub-themes with ≥10 sets",
    x        = "β (slope)",
    y        = NULL,
    caption  = "Blue = diversity increases with set size  |  Red = decreases"
  ) +
  theme_sw

save_png(p4, "sw_p4_subtheme_slopes.png", w = 12, h = 7)

# ── 13. Plot 5 – Evenness vs. Shannon coloured by set size ────
p5 <- sw_diversity %>%
  drop_na(evenness) %>%
  ggplot(aes(x = shannon_parts, y = evenness, colour = log10(total_parts))) +
  geom_point(alpha = 0.5, size = 2) +
  scale_colour_gradient(low = sw_blue, high = sw_red,
                        name = "log₁₀(parts)",
                        labels = function(x) round(10^x)) +
  labs(
    title    = "Shannon Diversity vs. Pielou's Evenness",
    subtitle = "Point colour = set size  |  High H′ + high J → many part types used roughly equally",
    x        = "Shannon diversity H′",
    y        = "Pielou's evenness J",
    caption  = "Source: Rebrickable"
  ) +
  theme_sw

save_png(p5, "sw_p5_shannon_vs_evenness.png")

# ── 14. Save tidy results CSV ─────────────────────────────────
out_csv <- file.path(data_dir, "sw_diversity_metrics.csv")
write_csv(sw_diversity, out_csv)
message(sprintf("\nResults saved to: %s", out_csv))
message(sprintf("Plots saved to:   %s", data_dir))
message("\nDone.")
