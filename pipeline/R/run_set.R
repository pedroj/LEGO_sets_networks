#!/usr/bin/env Rscript
# One entry point per set. Run from the repo root:
#   Rscript pipeline/R/run_set.R <set> [steps]
# steps (comma-separated, default "audit,plot,xlsx"):
#   instances  parse the MPD -> _instances.csv, _node_coords.csv, _kernel.csv (+ starting matrix if missing)
#   bboxes     LDraw bounding boxes of every part -> part_bbox.csv          (python, pipeline/py/make_bboxes.py)
#   audit      geometry + compatibility + hand checks -> _checked.csv, _link_audit.csv/.md   (python)
#   promote    back up the starting matrix to bckp/ and replace it with _checked.csv
#   plot       network figures from <set>_adjacency_matrix.csv -> _network*.pdf/png, _nodes.csv, _edges.csv
#   xlsx       adjacency matrix with icons -> _adjacency_matrix_icons.xlsx (python)
#   all        instances,bboxes,audit,promote,plot,xlsx
# Set-specific input lives only in output/<set>_lineart/set_config.yaml.
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("usage: Rscript pipeline/R/run_set.R <set> [steps]")
S <- args[1]
steps <- strsplit(if (length(args) > 1) args[2] else "audit,plot,xlsx", ",")[[1]]
if ("all" %in% steps) steps <- c("instances", "bboxes", "audit", "promote", "plot", "xlsx")
root <- normalizePath(".")
RDIR <- file.path(root, "pipeline", "R"); PYDIR <- file.path(root, "pipeline", "py")
setdir <- file.path(root, "output", paste0(S, "_lineart"))
stopifnot("set folder not found" = dir.exists(setdir), "set_config.yaml missing" = file.exists(file.path(setdir, "set_config.yaml")))
setwd(setdir)
source(file.path(RDIR, "set_config.R")); cfg <- read_set_config()
py <- function(script, ...) {
  cat(sprintf("\n== python %s\n", script))
  st <- system2("python3", c(shQuote(file.path(PYDIR, script)), ...))
  if (st != 0) stop(script, " failed (exit ", st, ")")
}
stamp <- format(Sys.time(), "%Y%m%d_%H%M")
for (st in steps) {
  cat(sprintf("\n######## %s: %s\n", S, st))
  switch(st,
    instances = { source(file.path(RDIR, "instances.R")); make_instances(cfg, RDIR) },
    bboxes    = py("make_bboxes.py", "parts_list.txt", "part_bbox.csv"),
    audit     = { py("check_geometry.py"); py("check_with_instructions.py") },
    promote   = {
      start <- paste0(S, "_adjacency_matrix.csv"); chk <- paste0(S, "_adjacency_matrix_checked.csv")
      dir.create("bckp", showWarnings = FALSE)
      file.copy(start, file.path("bckp", sprintf("%s_adjacency_matrix_%s.csv", S, stamp)))
      file.copy(chk, start, overwrite = TRUE); cat("promoted", chk, "->", start, "(old copy in bckp/)\n") },
    plot      = {
      source(file.path(RDIR, "plot_lego_network.R")); apply_set_config(cfg)
      compat <- file.path(root, "data", "lego_compat_out", "compat_matrix_subtype_codes.csv")
      icons <- file.path(root, "data", "icons")
      title <- if (!is.null(cfg$plot$panel_title)) cfg$plot$panel_title else cfg$name
      plot_lego_network(paste0(S, "_adjacency_matrix.csv"), icons, set_info = "set_info.json",
                        parts_meta = "rebrickable/set_parts.csv", out_prefix = S, label = "part", compat_csv = compat)
      if (file.exists(paste0(S, "_icons.pdf"))) file.rename(paste0(S, "_icons.pdf"), paste0(S, "_network.pdf"))
      plot_lego_network_coords(paste0(S, "_adjacency_matrix.csv"), paste0(S, "_node_coords.csv"), icons,
                               parts_meta = "rebrickable/set_parts.csv", out_prefix = S, set_info = "set_info.json",
                               panel_title = title, compat_csv = compat)
      if (file.exists(paste0(S, "_network_coords_icons.png"))) file.rename(paste0(S, "_network_coords_icons.png"), paste0(S, "_network_coords.png")) },
    xlsx      = py("build_adjacency_xlsx.py"),
    stop("unknown step: ", st))
}
cat("\ndone:", S, paste(steps, collapse = ","), "\n")
