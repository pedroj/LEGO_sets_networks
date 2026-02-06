# Test for LEGO network analysis.
library(dplyr)
library(readr)
library(stringr)
library(purrr)

ldr_path <- "example.ldr"

# Read all lines, keep only type-1 lines
ldr_lines <- read_lines(ldr_path)
part_lines <- ldr_lines[str_starts(str_trim(ldr_lines), "1 ")]

# Parse each type-1 line into fields
parse_part_line <- function(line) {
    # split on whitespace
    tok <- str_split(str_trim(line), "\\s+", simplify = TRUE)
    # tokens:
    # 1: "1"
    # 2: color
    # 3-5: x y z
    # 6-14: r11..r33
    # 15: part_id
    tibble(
        color = tok[2],
        x = as.numeric(tok[3]),
        y = as.numeric(tok[4]),
        z = as.numeric(tok[5]),
        r11 = as.numeric(tok[6]),
        r12 = as.numeric(tok[7]),
        r13 = as.numeric(tok[8]),
        r21 = as.numeric(tok[9]),
        r22 = as.numeric(tok[10]),
        r23 = as.numeric(tok[11]),
        r31 = as.numeric(tok[12]),
        r32 = as.numeric(tok[13]),
        r33 = as.numeric(tok[14]),
        part_id = tok[15]
    )
}

model_parts <- map_dfr(part_lines, parse_part_line) %>%
    mutate(instance_id = paste0("p", row_number())) %>%
    select(instance_id, part_id, color, x, y, z,
           r11, r12, r13, r21, r22, r23, r31, r32, r33)

model_parts # First datafile for workflow.

library(tibble)

# 2x4 brick (3001.dat) connector catalog, local coordinates
# Reference frame:
# - x: width direction (2 studs → 0 and 20)
# - z: length direction (4 studs → 0, 20, 40, 60)
# - y: up; top studs at y = 0, tubes slightly below (e.g. y = -9.6)

# Second datafile for workflow.
connector_catalog <- tribble(
    ~part_id,    ~connector_id, ~type,    ~x,  ~y,   ~z,
    # Top studs (8)
    "3001.dat",  "3001_S1",     "stud",    0,  0,    0,
    "3001.dat",  "3001_S2",     "stud",   20,  0,    0,
    "3001.dat",  "3001_S3",     "stud",    0,  0,   20,
    "3001.dat",  "3001_S4",     "stud",   20,  0,   20,
    "3001.dat",  "3001_S5",     "stud",    0,  0,   40,
    "3001.dat",  "3001_S6",     "stud",   20,  0,   40,
    "3001.dat",  "3001_S7",     "stud",    0,  0,   60,
    "3001.dat",  "3001_S8",     "stud",   20,  0,   60,
    
    # Underside tubes (3 internal tubes for a 2x4 brick)
    # Approximate positions: centered between studs
    # y slightly below studs; z between rows
    "3001.dat",  "3001_T1",     "tube",   10, -9.6, 10,
    "3001.dat",  "3001_T2",     "tube",   10, -9.6, 30,
    "3001.dat",  "3001_T3",     "tube",   10, -9.6, 50
)

# 