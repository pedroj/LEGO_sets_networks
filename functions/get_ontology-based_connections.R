# 1. Define a tiny MPD in R
# We use three identical bricks (file 3001.dat, a 2×4 brick) stacked at z = 0, 24, 48 LDU (i.e. three brick heights). All rotations are identity.
mpd_text <- "
0 FILE toy.mpd
0 !LDRAW_ORG Model
1 16 0 0 0  1 0 0 0 1 0 0 0 1 3001.dat
1 16 0 24 0 1 0 0 0 1 0 0 0 1 3001.dat
1 16 0 48 0 1 0 0 0 1 0 0 0 1 3001.dat
"
writeLines(mpd_text, "toy.mpd")

# LDraw spec: type‑1 lines are 1 <colour> x y z a b c d e f g h i <file>.


# 2. Parse part instances

library(dplyr)
library(stringr)
library(readr)
library(here)
mpd <- read_lines("toy.mpd")

# Main model block: lines after first 0 FILE
file_start <- which(str_detect(mpd, "^0 FILE"))[1]
main_block <- mpd[(file_start+1):length(mpd)]

# Type-1 lines only
part_lines <- main_block[str_detect(main_block, "^1 ")]

parts <- str_split_fixed(part_lines, "\\s+", 15) |> as.data.frame()
names(parts) <- c("type","col","x","y","z","a","b","c","d","e","f","g","h","i","file")

parts <- parts %>%
    mutate(
        id   = row_number(),
        x    = as.numeric(x),
        y    = as.numeric(y),
        z    = as.numeric(z),
        file = basename(file)
    )

parts

# We now have three part instances with ids 1–3 at z = 0, 24, 48.

# 3. Very simple connection inference (stacked bricks)
# For this toy example, we assume:
#     
#     All bricks are axis‑aligned (identity rotation).
# 
# A connection exists between two parts if they share x,y and differ by exactly 24 LDU in z (one brick height).
# 
# We then label that connection as “Stud to Antistud Tube” from your ontology.
# All unordered pairs of parts
# parts must at least have: id, x, y, z
# e.g. from the toy example
# parts <- data.frame(id = 1:3,
#                     x = c(0,0,0),
#                     y = c(0,24,48),
#                     z = c(0,0,0))

pairs <- tidyr::crossing(
    p1 = parts$id,
    p2 = parts$id
) %>%
    dplyr::filter(p1 < p2) %>%
    dplyr::left_join(parts %>% select(id, x, y, z), by = c("p1" = "id")) %>%
    dplyr::rename(x1 = x, y1 = y, z1 = z) %>%
    dplyr::left_join(parts %>% select(id, x, y, z), by = c("p2" = "id")) %>%
    dplyr::rename(x2 = x, y2 = y, z2 = z)

names(pairs)
head(pairs)

brick_height <- 24

edges <- pairs %>%
    dplyr::filter(
        x1 == x2,
        z1 == z2,
        abs(y1 - y2) == brick_height
    ) %>%
    dplyr::transmute(
        from = p1,
        to   = p2,
        Connection_Type = "Stud to Antistud Tube"
    )
nrow(edges)
edges

# For this toy MPD, you should get edges 1–2 and 2–3, representing vertical stud–tube connections between stacked bricks.


# 4. Build igraph from parts and edges

library(igraph)
g <- graph_from_data_frame(
    d = edges,
    vertices = parts %>% select(id, file, x, y, z),
    directed = FALSE
)

E(g)$connection_type <- edges$Connection_Type

g


plot(g, vertex.label = V(g)$id)

# graph_from_data_frame() automatically uses the first two columns of edges as the edge list and attaches the rest as edge attributes. Nodes carry part‑level attributes (file name and coordinates).

# 5. Optional: integrate the CSV ontology
# If you put lego_connections_ontology.csv in the working directory, you can join it to enrich edges with family and geometry.

ontology <- read.csv(here::here("data/extdata/lego_connections_ontology.csv"), stringsAsFactors = FALSE)

edges2 <- edges %>%
    left_join(ontology, by = c("Connection_Type" = "Connection_Type"))

g <- graph_from_data_frame(
    d = edges2,
    vertices = parts %>% select(id, file, x, y, z),
    directed = FALSE
)

E(g)$family <- edges2$Connection_Family
E(g)$primary_geom <- edges2$Primary_Geometry_mm

# You now have a tiny but fully working pipeline: MPD → parts → edges → igraph, with connection types grounded in your ontology.



