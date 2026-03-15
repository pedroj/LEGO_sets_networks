# Test without minifigures and submodels
# 1. Build a primitives‑only graph from the MPD
# Add a filter right after parts_all is built, and remove submodels/minifigs via a small exclusion table or regex.
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(here)
library(stringr)
library(tidyr)
library(igraph)
library(ggnetwork); library(intergraph)
library(igraph)
library(ggraph)
library(ggplot2)
library(GGally)

library(jsonlite)  # For API
library(purrr)
library(httr)
library(ggimage)
library(grid)
library(colouR)
library("png")                      # Load png package
# install.packages("patchwork")       # Install patchwork package
library("patchwork")                # Load patchwork

library(tidygraph)

# 1) Read a .mpd file for the selected LEGO set.
set_id<- "4489-1" ###### <<<<<------- Input set code here.
set_num<- set_id
# mpd_lines <- readLines(here::here("data/models/601-2-Tow_Truck.mpd"))
# For Star Wars models
mpd_lines <- readLines(here::here(paste0("data/models/starwars/",set_id,".mpd")))

# Helper: extract setname.
source(here::here("functions/extract_mpd_set_name.R"))
# setname<- extract_mpd_set_name(here::here("data/models/starwars/7131-2-PJ.mpd"))
setname<- extract_mpd_set_name(here::here(paste0("data/models/starwars/",set_id,".mpd")))

# readPNG function to load an image of the set that we have stored on our computer:
# For later use as inset in a network graph.
my_image <- readPNG(here::here(paste0("images/starwars_sets_images/",setname,".png"))) #, 
#  native = TRUE))
set_image_row <- sets %>%
    dplyr::filter(set_num == set_id)

set_image_url <- set_image_row$img_url

# Helper: identify FILE sections
file_idx  <- grep("^0 FILE", mpd_lines)
file_idx_end <- c(file_idx[-1] - 1, length(mpd_lines))

files_df <- tibble(
    file_start = file_idx,
    file_end   = file_idx_end,
    file_header = mpd_lines[file_idx]
) %>%
    mutate(
        file_name = str_trim(str_remove(file_header, "^0 FILE")),
        file_name = str_replace(file_name, "^\\d+\\s+-\\s+", "")  # strip leading "106 - "
    )

files_df

# Function to parse "1 ..." lines inside a FILE block
parse_file_parts <- function(lines, file_name) {
    # LDraw "1" line: 1 col x y z a b c d e f g h i part.dat or subfile.ldr
    part_lines_idx <- grep("^1\\s", lines)
    if (length(part_lines_idx) == 0) return(NULL)
    
    # Split by whitespace
    tokens <- str_split(lines[part_lines_idx], "\\s+", simplify = FALSE)
    
    # Map to data frame
    parts <- lapply(tokens, function(tok) {
        # minimum length check
        if (length(tok) < 15) return(NULL)
        tibble(
            file      = file_name,
            color     = tok[2],
            x         = as.numeric(tok[3]),
            y         = as.numeric(tok[4]),
            z         = as.numeric(tok[5]),
            a         = as.numeric(tok[6]),
            b         = as.numeric(tok[7]),
            c         = as.numeric(tok[8]),
            d         = as.numeric(tok[9]),
            e         = as.numeric(tok[10]),
            f         = as.numeric(tok[11]),
            g         = as.numeric(tok[12]),
            h         = as.numeric(tok[13]),
            i         = as.numeric(tok[14]),
            part_raw  = paste(tok[15:length(tok)], collapse = " ")
        )
    })
    
    bind_rows(parts)
}

# Apply to each FILE block
parts_all <- purrr::map2_dfr(
    files_df$file_start,
    files_df$file_end,
    ~{
        block_lines <- mpd_lines[.x:.y]
        fname <- files_df$file_name[files_df$file_start == .x]
        parse_file_parts(block_lines, fname)
    }
)

parts_all

## After parts_all is created ------------------------------------------

# Clean part name
parts_all <- parts_all %>%
    mutate(
        part = str_trim(part_raw)
    )

# Identify file type
parts_all <- parts_all %>%
    mutate(
        is_submodel  = str_detect(part, "\\.ldr$"),
        is_primitive = str_detect(part, "\\.dat$")
    )

# Optional: drop minifig-related primitives by a simple pattern or list
minifig_patterns <- c("minifig", "head", "torso", "hip", "leg", "arm", "hand")
parts_all <- parts_all %>%
    filter(is_primitive) %>%                                # keep only .dat
    filter(!str_detect(str_to_lower(part), 
                       paste(minifig_patterns, collapse = "|")))

# Co‑occurrence edges among primitives only
edges_df <- parts_all %>%
    group_by(file) %>%
    summarise(parts = list(unique(part)), .groups = "drop") %>%
    mutate(
        edges = purrr::map(parts, ~{
            p <- .x
            if (length(p) < 2) return(NULL)
            comb <- t(combn(p, 2))
            tibble(from = comb[,1], to = comb[,2])
        })
    ) %>%
    select(edges) %>%
    unnest(edges)

# Graph of primitive parts only
g <- graph_from_data_frame(edges_df, directed = FALSE)

# Add part_id (strip .dat) for joining to Rebrickable parts list
V(g)$part_id <- sub("\\.dat$", "", V(g)$name)

# At this point, g is the primitive‑part co‑occurrence network, with minifig pieces and submodels excluded.
# 
# 2. Download/cache part images and attach paths to vertices
# You already get target_set_parts and construct local paths with get_LEGO_part_images_for_set(). Here we only ensure we have one row per part and join by part_id.


## Download + cache images (as you already do) -------------------------

target_set <- set_id
api_key    <- "40c6692458e8b4c27074486ab114d6cc"

source(here::here("functions/get_LEGO_set_parts.R"))
target_set_parts <- get_LEGO_set_parts(target_set, api_key)

source(here::here("functions/get_LEGO_part_images_for_set.R"))
target_set_parts <- get_LEGO_part_images_for_set(target_set)
# Here I'm fixing the url for parts 4592 and 4593 which don't show up.
target_set_parts <- target_set_parts %>%
    mutate(
        img_local_path = case_when(
            part_num == "4592" ~ "images/part_images/4592.png",  # example
            part_num == "4593" ~ "images/part_images/4593.png",
            TRUE               ~ img_local_path
        )
    )

# One row per part_num (you may want per part_num+color_id if you care about colors)
target_set_parts <- target_set_parts %>%
    distinct(part_num, .keep_all = TRUE)

# Attach local img path to vertices (by part_id)
nodes_tbl <- tibble(part_id = V(g)$part_id) %>%
    left_join(
        target_set_parts %>% 
            select(part_num, img_local_path),
        by = c("part_id" = "part_num")
    )

# Sanity check: same number of rows
stopifnot(nrow(nodes_tbl) == vcount(g))

# Store image path back on the graph for ggraph/ggimage
V(g)$img_path <- nodes_tbl$img_local_path

# Assuming get_LEGO_part_images_for_set() actually downloads to images/part_images/... and populates img_local_path, this gives every primitive node a PNG path on disk.
# 
# 3. Plot g with small part images as nodes
# ggimage::geom_image() works fine with ggraph if we provide x–y coordinates via a layout data frame and then map image = img_path.

# Convert to tbl_graph to make node attributes tidygraph‑friendly
tg <- as_tbl_graph(g)

# Compute layout (Fruchterman–Reingold) once
tg <- as_tbl_graph(g)
set.seed(123)
layout_df <- create_layout(tg, layout = "fr")   # contains x, y, part_id, img_path

# layout_df is a data.frame with x, y and node variables (name, part_id, img_path, ...)
# Now plot with images as nodes
gg_lego <- ggraph(layout_df) +
    geom_edge_bend0(width = 0.38, alpha = 0.4) +
    # optional invisible points to define node size/legend
    geom_node_point(size = 0, alpha = 0) +
    geom_image(
        aes(x = x, y = y, image = img_path),
        size = 0.05,
        inherit.aes = FALSE
    ) +
    geom_node_text(
        aes(label = part_id),
#        nudge_y = -0.03,     # small negative shift: just below image
        vjust = 4.25,
        size = 5,
        size    = 3,
        color   = "black"
    ) +
    theme_graph() +
    labs(title = paste0("LEGO Parts Network ", setname))

gg_lego

# If you still want the set box image inset:
my_image_array <- readPNG(here::here(
    paste0("images/starwars_sets_images/", setname, ".png")
))

p_logo <- ggplot() +
    annotation_custom(
        rasterGrob(my_image_array, interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    ) +
    theme_void()

gg_lego_inset <- gg_lego +
    patchwork::inset_element(
        p_logo,
        left   = 0.05,
        bottom = 0.5,
        right  = 0.19,
        top    = 0.8,
        align_to = "full",
        clip     = TRUE
    )

gg_lego_inset

