# Scheme code. Plug the real data into this workflow.
# 
#----------------------------------------------------------------
# 1. Data structures in R
# Connector profiles per part
library(dplyr)
library(purrr)
library(readr)

# Connector catalog: for each LDraw part, all connector points in LOCAL coords
# type could be: "stud", "tube", "pin", "pin_hole", "axle", "axle_hole", etc.
# connector_catalog <- tibble::tibble(
#     part_id = character(),
#     connector_id = character(),
#     type = character(),
#     x = double(),
#     y = double(),
#     z = double()
# )

# Example dummy data for illustration
# connector_catalog <- tibble::tribble(
#     ~part_id, ~connector_id, ~type,  ~x,  ~y,  ~z,
#     "3001.dat", "3001_S1",  "stud",  0,   0,   0,
#     "3001.dat", "3001_S2",  "stud", 20,   0,   0,
#     "3001.dat", "3001_T1",  "tube",  0,   0,  -8,
#     "3020.dat", "3020_T1",  "tube",  0,   0,  -8,
#     "3020.dat", "3020_S1",  "stud",  0,   0,   0
# )

# Simple compatibility map between connector types
# Which connector types can connect to which (undirected)
connector_compat <- tibble::tribble(
    ~type_a,  ~type_b,
    "stud",   "tube",
    "tube",   "stud",
    "pin",    "pin_hole",
    "pin_hole","pin",
    "axle",   "axle_hole",
    "axle_hole","axle"
)

#----------------------------------------------------------------
# 2. Parse an LDraw model into parts + transforms
# Assume you have a small helper that reads an .ldr file and returns a parts table:
    
# Minimal representation of a part instance in a model
# Each row = one placed part, with 3D position and a 3x3 rotation matrix
# model_parts <- tibble::tribble(
#     ~instance_id, ~part_id,   ~x,  ~y,  ~z,
#     ~r11, ~r12, ~r13, ~r21, ~r22, ~r23, ~r31, ~r32, ~r33,
#     "p1", "3001.dat",  0,   0,   0,
#     1,   0,   0,  0,   1,   0,  0,   0,   1,
#     "p2", "3020.dat",  0,   0,  -8,
#     1,   0,   0,  0,   1,   0,  0,   0,   1
# )

# In real code, write a parser that:
# - reads "1 ..." lines from the LDraw file
# - extracts part_id, position (x,y,z), and rotation matrix (r11..r33)

#----------------------------------------------------------------
# 3. Transform connectors to world coordinates

# Helper: apply rotation + translation to a single connector
transform_connector <- function(cx, cy, cz,
                                px, py, pz,
                                r11, r12, r13,
                                r21, r22, r23,
                                r31, r32, r33) {
    wx <- r11 * cx + r12 * cy + r13 * cz + px
    wy <- r21 * cx + r22 * cy + r23 * cz + py
    wz <- r31 * cx + r32 * cy + r33 * cz + pz
    c(wx, wy, wz)
}

# Expand model_parts × connector_catalog and compute world coords
world_connectors <- model_parts %>%
    inner_join(connector_catalog, by = "part_id") %>%
    rowwise() %>%
    mutate(
        world = list(
            transform_connector(
                cx = x.y, cy = y.y, cz = z.y,   # connector local coords
                px = x.x, py = y.x, pz = z.x,   # part position
                r11 = r11, r12 = r12, r13 = r13,
                r21 = r21, r22 = r22, r23 = r23,
                r31 = r31, r32 = r32, r33 = r33
            )
        ),
        wx = world[1],
        wy = world[2],
        wz = world[3]
    ) %>%
    ungroup() %>%
    transmute(
        instance_id,
        part_id,
        connector_id,
        type,
        wx, wy, wz
    )
# Note: because we joined model_parts and connector_catalog, x.x is the part position and x.y the connector local x; adapt names to your actual code.

#----------------------------------------------------------------
# 4. Detect connections between parts
# We now:
#     
#     Compare pairs of connectors from different instances.
# 
# Keep those within a distance tolerance and with compatible types.

# Distance tolerance in LDraw units (tune to real geometry)
tol <- 50.0 #0.75

# Self-join world_connectors to find close, compatible pairs
connector_pairs <- world_connectors %>%
    dplyr::rename(
        inst_a = instance_id,
        part_a = part_id,
        conn_a = connector_id,
        type_a = type,
        wx_a = wx, wy_a = wy, wz_a = wz
    ) 
connector_pairs <- connector_pairs %>%
    dplyr::mutate(dummy = 1) %>%
    dplyr::inner_join(
        world_connectors %>%
            rename(
                inst_b = instance_id,
                part_b = part_id,
                conn_b = connector_id,
                type_b = type,
                wx_b = wx, wy_b = wy, wz_b = wz
            ) %>%
            mutate(dummy = 1),
        by = "dummy",
        suffix = c("_a", "_b")
    ) 
connector_pairs <- connector_pairs %>%
    dplyr::filter(inst_a != inst_b) %>% # different part
                                        # instances
    dplyr::select(-dummy) %>%
    # avoid double counting A-B and B-A
    dplyr::filter(inst_a < inst_b) %>%
    # compute distance
    rowwise() 
connector_pairs <- connector_pairs %>%
    dplyr::mutate(
        dist = sqrt((wx_a - wx_b)^2 + (wy_a - wy_b)^2 + (wz_a - wz_b)^2)
    ) %>%
    ungroup() %>%
    # close enough to consider "connected"
    dplyr::filter(dist <= tol) %>%
    # types must be compatible
    dplyr::inner_join(connector_compat, 
                      by = c("type_a", "type_b"))

# At this point connector_pairs is at connector‑level. You can collapse to part‑level edges.
# 

#----------------------------------------------------------------
# 5. Export part–part connection CSV

part_edges <- connector_pairs %>%
    transmute(
        part_instance_a = inst_a,
        part_id_a       = part_a,
        part_instance_b = inst_b,
        part_id_b       = part_b,
        connection_type = paste(type_a, type_b, sep = "_"),
        distance        = dist
    ) %>%
    distinct()

# Write to CSV
write_csv(part_edges, "ldraw_model_connedgelist.csv")

# This yields a file like:
#     
#     text
# part_instance_a,part_id_a,part_instance_b,part_id_b,connection_type,distance
# p1,3001.dat,p2,3020.dat,stud_tube,0.12
# ...
# 

#----------------------------------------------------------------
# 6. How you’d plug this into real data
# Replace the dummy connector_catalog with a table extracted 
# from:
#     A community connection DB (e.g. JBrickBuilder’s connection model) or
# 
# Your own manual annotation of connector positions per part.
# 
# Replace the stub model_parts with a parser that reads 1‑type lines from a real *.ldr / *.mpd into instance_id, part_id, x,y,z, r11..r33.
# 
# Tune tol and connector_compat based on real LDraw units and connector geometry.
# 
