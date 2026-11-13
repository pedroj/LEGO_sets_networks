# Interactive HTML adjacency widget (the "artifact") for one set, built from an adjacency matrix.
# Uses write_adjacency_html() and HTML_TEMPLATE from the vendored kernel.R, so every set gets the same design
# (inline #ttip tooltip, stat cards, category filters, degree bar). It does NOT need the instances CSV to be in the
# same order as the matrix: node names (<dat>#<k>) are read from the matrix itself.
# Part names: <set>_nodes.csv (desc) > shared parts catalogue (data/extdata/rebrickable-repo/parts_catalog.csv) > <set>_instances.csv (nm, truncated to 40 chars).
# Family (category colour): <set>_instances.csv (fam), else lower-cased category of <set>_nodes.csv, else "other".
FAM_COL_WIDGET <- c(brick = "#4E79A7", plate = "#59A14F", tile = "#8CD17D", slope = "#B07AA1",
                    round = "#499894", cone = "#86BCB6", baseplate = "#A0CBE8",
                    panel = "#F1CE63", bracket = "#B6992D", technic = "#E15759",
                    wheel = "#79706E", wheel_holder = "#D37295", tyre = "#4B4B4B",
                    minifig = "#FF9D9A", other = "#BAB0AC")
make_widget <- function(cfg, pipeline_dir, matrix_csv = NULL, out_prefix = NULL) {
  source(file.path(pipeline_dir, "kernel.R"), local = FALSE)
  S <- cfg$set; if (is.null(out_prefix)) out_prefix <- S
  if (is.null(matrix_csv)) matrix_csv <- paste0(S, "_adjacency_matrix.csv")
  A <- as.matrix(read.csv(matrix_csv, row.names = 1, check.names = FALSE)); storage.mode(A) <- "integer"
  A[A > 0L] <- 1L
  node <- rownames(A); part_num <- sub("#.*$", "", node)
  alias <- c("64567" = "577b", "577b" = "64567")                    # LDraw <-> Rebrickable numbering seen in 75344
  look <- function(tab, key, col) { i <- match(key, tab$k); j <- is.na(i) & key %in% names(alias); i[j] <- match(alias[key[j]], tab$k); tab[[col]][i] }
  nm <- rep(NA_character_, length(node)); fam <- rep(NA_character_, length(node))
  rd <- function(f) if (file.exists(f)) read.csv(f, check.names = FALSE, colClasses = "character") else NULL
  inst <- rd(paste0(S, "_instances.csv")); nod <- rd(paste0(S, "_nodes.csv")); sp <- rd(if (nzchar(Sys.getenv("LEGO_PARTS_CATALOG"))) Sys.getenv("LEGO_PARTS_CATALOG") else "rebrickable/set_parts.csv")
  if (!is.null(nod)) { t <- data.frame(k = nod$part, desc = nod$desc, cat = tolower(nod$category)); nm <- look(t, part_num, "desc"); fam <- look(t, part_num, "cat") }
  if (!is.null(sp))  { t <- data.frame(k = sp$part_num, v = sp$part_name); t <- t[!duplicated(t$k), ]; nm <- ifelse(is.na(nm), look(t, part_num, "v"), nm) }
  if (!is.null(inst)) {
    t <- inst[!duplicated(inst$dat), ]; t <- data.frame(k = t$dat, nm = t$nm, fam = t$fam)
    nm <- ifelse(is.na(nm), look(t, part_num, "nm"), nm); fi <- look(t, part_num, "fam"); fam <- ifelse(is.na(fi), fam, fi)
  }
  fam[is.na(fam) | !fam %in% names(FAM_COL_WIDGET)] <- "other"      # defensive: NA -> "other"
  title <- if (!is.null(cfg$plot$widget_title)) cfg$plot$widget_title else S
  write_adjacency_html(A, node, part_num, fam, nm, crit = NULL, out_prefix = out_prefix, FAM_COL = FAM_COL_WIDGET,
                       title = title, theme = sprintf("%d part types", length(unique(part_num))),
                       source_label = "MPD/LDR parsed, links checked")
}
