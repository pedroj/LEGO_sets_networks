K <- Sys.glob("~/.claude/skills/synced/*/lego-mpd-adjacency/kernel.R")[1]
source(K)
.exp <- expand_model
is_prim <- function(d) grepl("^(stug|stud|rect|box|4-4|4-8|2-4|1-4|3-8|1-8|4-1|s/)", tolower(d)) | grepl("^[0-9]+-[0-9]+", d) | grepl("^(stud|stug)", tolower(d))
expand_model <- function(...) Filter(function(p) !is_prim(p$dat), .exp(...))
res <- run_lego_adjacency("4010_-_Police_rescue_boat.mpd", "rebrickable", "4010")
