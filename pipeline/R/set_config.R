# Read output/<set>_lineart/set_config.yaml and expose it to the plotting code.
# plot_lego_network.R looks for the globals .PART_OVERRIDE and .PAIR_ALLOW; they are built here from the config,
# so the per-set corrections are written once (set_config.yaml) and used by both the Python audit and the R plots.
read_set_config <- function(path = "set_config.yaml") {
  cfg <- yaml::read_yaml(path)
  cfg$set <- as.character(cfg$set)
  cfg
}
apply_set_config <- function(cfg, env = globalenv()) {
  so <- cfg$subtype_override; co <- cfg$category_override
  if (length(so)) {
    parts <- names(so)
    assign(".PART_OVERRIDE", data.frame(part = parts,
           category = vapply(parts, function(p) if (!is.null(co[[p]])) co[[p]] else NA_character_, ""),
           subtype = unlist(so, use.names = FALSE), stringsAsFactors = FALSE, row.names = NULL), envir = env)
  } else assign(".PART_OVERRIDE", data.frame(part = character(0), category = character(0), subtype = character(0)), envir = env)
  pa <- c(cfg$pair_allow, cfg$pair_allow_plot_only)   # plot_only: joints the R plots allow but the Python audit does not
  if (length(pa)) {
    key <- vapply(pa, function(x) paste(sort(as.character(unlist(x[1:2]))), collapse = "|"), "")
    assign(".PAIR_ALLOW", setNames(vapply(pa, function(x) as.character(x[[3]]), ""), key), envir = env)
  } else assign(".PAIR_ALLOW", character(0), envir = env)
  invisible(cfg)
}
