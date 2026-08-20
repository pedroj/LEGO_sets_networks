# kernel.R - LEGO MPD -> part adjacency matrix (auto-loaded by skill lego-mpd-adjacency)
# Defines run_lego_adjacency(mpd_path, rebrickable_dir=NULL, out_prefix=NULL).
# See SKILL.md for the connection model (proximity AND family compatibility).

# --- Tunables (LDU = LDraw Units; 1 stud = 20 LDU wide, 1 brick = 24 tall) ---
STUD     <- 20    # LDU per stud (horizontal footprint pitch)
PLATE_H  <- 8     # plate / tile height
BRICK_H  <- 24    # brick height
TOL      <- 6     # tolerance (LDU) for "boxes touch"
STACK_DY <- 24    # max vertical gap counted as a stud-stack (one brick)
HUB_D    <- 60    # tyre-to-wheel-hub max centre distance

# --- 1. Parse MPD/LDR into named sub-assemblies -----------------------------
parse_mpd <- function(path) {
  # split a vector of LDraw lines into named "0 FILE <name>" blocks
  split_blocks <- function(lines) {
    subfiles <- list(); cur <- NULL; buf <- character(0)
    flush <- function() if (!is.null(cur)) subfiles[[cur]] <<- buf
    for (ln in lines) {
      s <- trimws(ln)
      if (startsWith(s, "0 FILE ")) { flush(); cur <- trimws(sub("^0 FILE ", "", s)); buf <- character(0) }
      else if (s == "0 NOFILE")     { flush(); cur <- NULL; buf <- character(0) }
      else if (!is.null(cur))       buf <- c(buf, s)
    }
    flush(); subfiles
  }

  lines    <- readLines(path, warn = FALSE)
  subfiles <- split_blocks(lines)
  if (length(subfiles) == 0) subfiles[["__root__"]] <- trimws(lines)  # single-model, no 0 FILE

  # --- resolve EXTERNAL sub-files (Studio/BrickLink split exports) -----------
  # A type-1 reference to "*.ldr"/"*.mpd" not defined inline is loaded from a
  # sibling file in the model's directory (case-insensitive), and its own
  # "0 FILE" blocks are merged in. Repeats until no new externals appear.
  base_dir  <- dirname(path)
  disk      <- list.files(base_dir)
  disk_lc   <- setNames(disk, tolower(disk))
  key       <- function(s) tolower(trimws(s))
  repeat {
    have <- key(names(subfiles))
    refs <- character(0)
    for (blk in subfiles) for (ln in blk) {
      p <- parse_type1(ln); if (is.null(p)) next
      if (grepl("\\.(ldr|mpd)$", p$file, ignore.case = TRUE)) refs <- c(refs, p$file)
    }
    refs <- unique(refs[!key(refs) %in% have])          # not already loaded
    added <- FALSE
    for (rf in refs) {
      hit <- if (key(rf) %in% names(disk_lc)) disk_lc[[key(rf)]] else NULL
      if (is.null(hit)) next                             # sibling file absent
      ext <- split_blocks(readLines(file.path(base_dir, hit), warn = FALSE))
      for (nm in names(ext)) if (!key(nm) %in% key(names(subfiles))) {
        subfiles[[nm]] <- ext[[nm]]; added <- TRUE
      }
    }
    if (!added) break
  }
  subfiles
}

parse_type1 <- function(ln) {
  t <- strsplit(trimws(ln), "\\s+")[[1]]
  if (length(t) < 15 || t[1] != "1") return(NULL)
  list(colour = suppressWarnings(as.integer(t[2])),
       t = as.numeric(t[3:5]),
       R = matrix(as.numeric(t[6:14]), 3, 3, byrow = TRUE),
       file = paste(t[15:length(t)], collapse = " "))
}

# --- 2. Recursively expand nested sub-files -> flat placed real parts --------
expand_model <- function(subfiles, root = NULL, R = diag(3), t = c(0,0,0),
                         step0 = 0L, seen = character(0),
                         group = NULL, depth = 0L) {
  key <- function(s) tolower(trimws(s))
  names_lc <- setNames(names(subfiles), key(names(subfiles)))
  if (is.null(root)) root <- names(subfiles)[1]
  rk <- key(root)
  if (!rk %in% names(names_lc) || rk %in% seen) return(list())
  seen <- c(seen, rk)
  out <- list(); step <- step0
  for (ln in subfiles[[ names_lc[[rk]] ]]) {
    if (grepl("^0 STEP", ln)) { step <- step + 1L; next }
    p <- parse_type1(ln); if (is.null(p)) next
    gt <- as.numeric(R %*% p$t + t)
    gR <- R %*% p$R
    is_sub <- key(p$file) %in% names(names_lc) ||
              grepl("\\.ldr$", p$file, ignore.case = TRUE)
    if (is_sub && key(p$file) %in% names(names_lc)) {
      # the FIRST level of sub-models below the root defines a part's "group"
      # (the named .mpd sub-assembly it belongs to); deeper levels inherit it
      child_group <- if (depth == 0L) sub("\\.(ldr|mpd)$", "", p$file, ignore.case = TRUE)
                     else group
      out <- c(out, expand_model(subfiles, p$file, gR, gt, step, seen,
                                 group = child_group, depth = depth + 1L))
    } else {
      dat <- sub("\\.dat$", "", p$file, ignore.case = TRUE)
      g   <- if (is.null(group)) "(root)" else group
      out <- c(out, list(list(dat = dat, colour = p$colour,
                              x = gt[1], y = gt[2], z = gt[3],
                              R = gR, step = step, group = g)))
    }
  }
  out
}

# --- 3. Part metadata from Rebrickable set_parts.csv ------------------------
load_part_meta <- function(rebrickable_dir) {
  meta <- new.env(parent = emptyenv())
  if (is.null(rebrickable_dir)) return(meta)
  f <- file.path(rebrickable_dir, "set_parts.csv")
  if (!file.exists(f)) { warning("set_parts.csv not found; names/categories unavailable"); return(meta) }
  dt <- data.table::fread(f, select = c("part_num", "part_name", "part_category"),
                          showProgress = FALSE)
  dt <- unique(dt, by = "part_num")
  for (i in seq_len(nrow(dt)))
    assign(dt$part_num[i], list(name = dt$part_name[i], cat = dt$part_category[i]), envir = meta)
  meta
}

resolve_meta <- function(dat, meta) {
  cands <- unique(c(dat,
                    sub("c[0-9]+$", "", dat),
                    sub("p.*$", "",   dat),
                    sub("[a-z].*$", "", dat)))
  cands <- cands[!is.na(cands) & nzchar(cands)]
  for (c in cands) if (exists(c, envir = meta, inherits = FALSE))
    return(get(c, envir = meta, inherits = FALSE))
  list(name = NA_character_, cat = NA_character_)
}

# --- 4. Family + bounding box from name/category (the generalisation) -------
family_of <- function(name, cat) {
  s <- tolower(paste(cat, name))
  # Wheel-related parts disambiguated by structural noun:
  #   "Brick/Plate ... with Wheels" -> wheel_holder (stud-connects AND holds a tyre)
  #   "Tyre ..." -> tyre ; bare "Wheel ..." (rim) -> wheel
  if (grepl("wheel|tyre|tire", s)) {
    if (grepl("brick|plate", s)) return("wheel_holder")
    if (grepl("tyre|tire", s))   return("tyre")
    return("wheel")
  }
  # word boundaries so structural parts don't false-match minifig tokens:
  #   "Headlight" !~ head, "Hinges, Arms..." !~ arm, "Side Handle" !~ hand.
  # \\bminifig (prefix only) still catches "Minifig Head/Hand" and any part
  # whose Rebrickable category is "Minifig ..." (hair, headwear, weapons).
  if (grepl("\\bminifig|\\bfigure\\b|\\btorso\\b|\\bhips\\b|\\barm\\b|\\bhand\\b|\\bhead\\b|\\bhat\\b|\\bcap\\b|\\bhelmet\\b|\\bleg\\b", s)) return("minifig")
  if (grepl("baseplate", s))                         return("baseplate")
  if (grepl("technic|axle|\\bpin\\b|gear|beam|bush", s)) return("technic")
  if (grepl("tile",  s))                             return("tile")
  if (grepl("slope|wedge|roof", s))                  return("slope")
  if (grepl("cone",  s))                             return("cone")
  if (grepl("round", s) & grepl("brick", s))         return("round")
  if (grepl("panel|windscreen|windshield|door|window|glass", s)) return("panel")
  if (grepl("bar|clip|bracket|hinge", s))            return("bracket")
  if (grepl("plate", s))                             return("plate")
  if (grepl("brick", s))                             return("brick")
  "other"
}

dims_of <- function(name, fam) {
  s <- tolower(paste(fam, name))
  h <- if (fam %in% c("brick","round","cone","slope","technic")) BRICK_H
       else if (fam == "wheel_holder") (if (grepl("brick", s)) BRICK_H else PLATE_H)
       else if (fam == "baseplate") PLATE_H
       else PLATE_H
  w <- STUD; d <- STUD
  if (!fam %in% c("tyre","wheel","minifig","other") && !is.na(name)) {
    m <- regmatches(name, regexpr("([0-9]+)\\s*[xX]\\s*([0-9]+)", name))
    if (length(m) == 1) {
      nn <- as.numeric(strsplit(m, "\\s*[xX]\\s*")[[1]])
      w <- nn[1] * STUD; d <- nn[2] * STUD
    }
  }
  if (fam == "tyre")  { w <- 30; d <- 30; h <- 16 }
  if (fam == "wheel") { w <- 30; d <- 30; h <- 16 }
  list(w = w, d = d, h = h)
}

# --- 5. Compatibility policy (editable - the "in addition to proximity" gate)
is_hub_pair <- function(fa, fb)
  setequal(c(fa,fb), c("tyre","wheel")) ||
  ("tyre"  %in% c(fa,fb) && "wheel_holder" %in% c(fa,fb)) ||
  ("wheel" %in% c(fa,fb) && "wheel_holder" %in% c(fa,fb))

compatible <- function(fa, fb) {
  stud_sys <- c("plate","tile","slope","brick","round","baseplate","cone",
                "panel","bracket","wheel_holder")
  if ("other" %in% c(fa, fb))               return(TRUE)   # permissive default
  if (fa %in% stud_sys && fb %in% stud_sys) return(TRUE)
  if (is_hub_pair(fa, fb))                  return(TRUE)
  if ("technic" %in% c(fa,fb) && any(c(fa,fb) %in% c(stud_sys,"technic"))) return(TRUE)
  if ("minifig" %in% c(fa,fb) && any(c(fa,fb) %in% c(stud_sys,"minifig"))) return(TRUE)
  if (fa == fb && fa != "tyre")             return(TRUE)
  FALSE
}

# --- 6. Build adjacency (proximity AND compatibility) -----------------------
build_adjacency <- function(parts, tol = TOL) {
  N <- length(parts)
  bx <- lapply(parts, function(p) {
    d <- p$dims; R <- p$R
    hx <- d$w/2; hz <- d$d/2
    ex <- abs(R[1,1])*hx + abs(R[1,3])*hz
    ez <- abs(R[3,1])*hx + abs(R[3,3])*hz
    list(xmin=p$x-ex, xmax=p$x+ex, zmin=p$z-ez, zmax=p$z+ez,
         ymin=p$y-d$h, ymax=p$y, cx=p$x, cy=p$y, cz=p$z, fam=p$fam)
  })
  touch <- function(a,b) (a$xmin-tol<=b$xmax && b$xmin-tol<=a$xmax &&
                          a$ymin-tol<=b$ymax && b$ymin-tol<=a$ymax &&
                          a$zmin-tol<=b$zmax && b$zmin-tol<=a$zmax)
  xz    <- function(a,b) (a$xmin-tol<=b$xmax && b$xmin-tol<=a$xmax &&
                          a$zmin-tol<=b$zmax && b$zmin-tol<=a$zmax)
  ygap  <- function(a,b) max(0, max(a$ymin-b$ymax, b$ymin-a$ymax))
  cdist <- function(a,b) sqrt((a$cx-b$cx)^2+(a$cy-b$cy)^2+(a$cz-b$cz)^2)

  A <- matrix(0L, N, N); crit <- matrix(NA_character_, N, N)
  for (i in seq_len(N)) for (j in seq_len(N)) {
    if (j <= i) next
    a <- bx[[i]]; b <- bx[[j]]
    if (!compatible(a$fam, b$fam)) next
    if (is_hub_pair(a$fam, b$fam)) {
      prox <- cdist(a,b) < HUB_D; c <- "tyre-hub"
    } else {
      tt <- touch(a,b); st <- xz(a,b) && ygap(a,b) <= STACK_DY
      prox <- tt || st; c <- if (tt) "touch" else if (st) "stack" else NA_character_
    }
    if (prox) { A[i,j] <- 1L; A[j,i] <- 1L; crit[i,j] <- c }
  }
  list(A = A, crit = crit)
}

# --- 8. Interactive HTML adjacency widget (type-level, self-contained) -------
# Mirrors the style/content of the previous LEGO_networks_cl analysis:
# a part-TYPE x part-TYPE adjacency matrix with category-filter buttons,
# hover tooltip, click-to-inspect info panel, stat cards, category legend,
# and a per-part connection-degree bar chart. Styled with Claude Science
# design tokens (var(--color-*)) so it themes with the surrounding UI.
# Family colours are threaded from FAM_COL so the widget matches the PNGs.

HTML_TEMPLATE <- r"---(<style>
#w{padding:0.75rem 0.5rem;font-family:var(--font-sans,sans-serif)}
.sr-only{position:absolute;width:1px;height:1px;overflow:hidden;clip:rect(0,0,0,0)}
#hdr{display:flex;align-items:center;gap:9px;flex-wrap:wrap;margin-bottom:0.75rem}
.bx{font-size:11px;padding:2px 9px;border-radius:var(--border-radius-md);font-weight:500}
.bi{background:#E6F1FB;color:#0C447C}
.bg{background:#EAF3DE;color:#27500A}
.ba{background:#FAEEDA;color:#633806}
#legend{display:flex;gap:11px;flex-wrap:wrap;font-size:11px;color:var(--color-text-secondary);margin-bottom:0.6rem;align-items:center}
.ld{width:10px;height:10px;border-radius:2px;display:inline-block;margin-right:3px;vertical-align:middle}
#frow{display:flex;gap:6px;flex-wrap:wrap;margin-bottom:0.6rem;align-items:center}
.fb{font-size:11px;padding:3px 9px;border-radius:var(--border-radius-md);border:0.5px solid var(--color-border-secondary);background:transparent;color:var(--color-text-secondary);cursor:pointer;transition:background .12s}
.fb:hover,.fb.on{background:#E6F1FB;color:#0C447C;border-color:transparent}
#mc{overflow-x:auto}
table{border-collapse:collapse}
td,th{padding:0}
.rl{font-size:8.5px;white-space:nowrap;padding-right:5px;text-align:right;cursor:pointer;min-width:105px;vertical-align:middle;transition:color .1s}
.cl{writing-mode:vertical-rl;transform:rotate(180deg);font-size:8.5px;white-space:nowrap;cursor:pointer;padding-bottom:3px;transition:color .1s}
.cell{width:12px;height:12px;border-radius:2px;cursor:pointer;box-sizing:border-box;transition:transform .1s}
.cell:hover{transform:scale(1.5);z-index:10;position:relative}
.self{background:var(--color-border-tertiary)}
#ttip{background:var(--color-background-secondary);border:0.5px solid var(--color-border-secondary);border-radius:var(--border-radius-md);padding:5px 9px;font-size:11px;line-height:1.5;min-height:24px;margin-bottom:5px}
#ttip b{font-weight:500;display:block}
#info{padding:0.6rem 0.9rem;border:0.5px solid var(--color-border-tertiary);border-radius:var(--border-radius-lg);font-size:11.5px;color:var(--color-text-secondary);background:var(--color-background-secondary);min-height:42px;line-height:1.6;margin-top:0.55rem}
#info strong{color:var(--color-text-primary);font-weight:500}
#stats{display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin-bottom:0.75rem}
.stat{background:var(--color-background-secondary);border-radius:var(--border-radius-md);padding:0.55rem 0.7rem;text-align:center}
.stat-n{font-size:19px;font-weight:500;color:var(--color-text-primary)}
.stat-l{font-size:11px;color:var(--color-text-secondary);margin-top:1px}
#deg-section{margin-top:0.9rem;padding-top:0.6rem;border-top:0.5px solid var(--color-border-tertiary)}
#deg-label{font-size:11px;color:var(--color-text-tertiary);margin-bottom:5px}
#degbar{display:flex;gap:2px;align-items:flex-end;height:60px}
.db-wrap{display:flex;flex-direction:column;align-items:center;flex:1;min-width:7px;cursor:pointer}
.db{border-radius:2px 2px 0 0;width:100%;opacity:.82;transition:opacity .12s}
.db:hover{opacity:1}
.dlb{font-size:7.5px;text-align:center;color:var(--color-text-tertiary);margin-top:2px}
</style>

<h2 class="sr-only">__SRTITLE__</h2>
<div id="w">

<div id="hdr">
  <span style="font-size:13px;font-weight:500;color:var(--color-text-primary)">__TITLE__</span>
  <span class="bx bi">__NPARTS__ part types</span>
  <span class="bx bi">__NCONN__ connections</span>
  <span class="bx bg">__THEME__</span>
  <span class="bx ba">__SOURCE__</span>
</div>

<div id="stats">
  <div class="stat"><div class="stat-n">__NPARTS__</div><div class="stat-l">unique parts</div></div>
  <div class="stat"><div class="stat-n">__NCONN__</div><div class="stat-l">connections</div></div>
  <div class="stat"><div class="stat-n">__NINST__</div><div class="stat-l">part instances</div></div>
  <div class="stat"><div class="stat-n">__NISO__</div><div class="stat-l">isolated parts</div></div>
</div>

<div id="legend">
__LEGEND__
</div>

<div id="frow"><span style="font-size:11px;color:var(--color-text-tertiary)">Filter by category:</span></div>
<div id="ttip">Hover a part label to highlight its row &mdash; click a cell to inspect the connection.</div>
<div id="mc"></div>
<div id="info">Click any coloured cell to see connection details here.</div>
<div id="deg-section">
  <div id="deg-label">Connection degree per part &mdash; click a bar for details</div>
  <div id="degbar"></div>
</div>

</div>

<script>
const D=__DATA__;

const CC=__CC__;
const n=D.parts.length;
let af=null,hl=null;

function blend(a,b){
  const hr=h=>[parseInt(h.slice(1,3),16),parseInt(h.slice(3,5),16),parseInt(h.slice(5,7),16)];
  const [ra,rb]=[hr(a),hr(b)];
  return`rgb(${Math.round((ra[0]+rb[0])/2)},${Math.round((ra[1]+rb[1])/2)},${Math.round((ra[2]+rb[2])/2)})`;
}
function deg(i){return D.matrix[i].reduce((s,v)=>s+v,0);}

const cats=[...new Set(D.cats)].sort();
const fr=document.getElementById('frow');
const ab=document.createElement('button');
ab.className='fb on'; ab.textContent='All';
ab.onclick=()=>{af=null; document.querySelectorAll('.fb').forEach(b=>b.classList.remove('on')); ab.classList.add('on'); render();};
fr.appendChild(ab);
cats.forEach(c=>{
  const b=document.createElement('button'); b.className='fb'; b.textContent=c;
  b.style.borderLeft=`3px solid ${CC[c]||'#888'}`;
  b.onclick=()=>{af=(af===c?null:c); document.querySelectorAll('.fb').forEach(x=>x.classList.remove('on')); (af?b:ab).classList.add('on'); render();};
  fr.appendChild(b);
});

function render(){
  const vis=af ? D.parts.map((_,i)=>D.cats[i]===af?i:null).filter(i=>i!==null) : D.parts.map((_,i)=>i);
  let h='<table><thead><tr><th style="min-width:105px"></th>';
  vis.forEach(i=>{h+=`<th><div class="cl" style="color:${CC[D.cats[i]]||'#888'}" data-i="${i}">${D.names[i]}</div></th>`;});
  h+='</tr></thead><tbody>';
  vis.forEach(ri=>{
    const rc=CC[D.cats[ri]]||'#888';
    const hlbg=hl===ri?`style="background:var(--color-background-secondary)"`:'';
    h+=`<tr ${hlbg}><td class="rl" style="color:${rc}" data-i="${ri}">${D.names[ri]}</td>`;
    vis.forEach(ci=>{
      if(ri===ci){h+=`<td><div class="cell self"></div></td>`;return;}
      const v=D.matrix[ri][ci];
      const bg=v?`background:${blend(rc,CC[D.cats[ci]]||'#888')}`:'background:transparent';
      h+=`<td><div class="cell" style="${bg}" data-r="${ri}" data-c="${ci}"></div></td>`;
    });
    h+='</tr>';
  });
  h+='</tbody></table>';
  document.getElementById('mc').innerHTML=h;
  document.querySelectorAll('[data-r]').forEach(el=>{
    el.addEventListener('click',()=>onCell(+el.dataset.r,+el.dataset.c));
    el.addEventListener('mouseenter',()=>onHov(+el.dataset.r,+el.dataset.c,!!D.matrix[+el.dataset.r][+el.dataset.c]));
    el.addEventListener('mouseleave',clearTip);
  });
  document.querySelectorAll('[data-i]').forEach(el=>{
    el.addEventListener('mouseenter',()=>{hl=+el.dataset.i; render();});
    el.addEventListener('mouseleave',()=>{hl=null; render();});
  });
  renderDeg(vis);
}

function onCell(r,c){
  const v=D.matrix[r][c];
  const rn=D.names[r], cn=D.names[c];
  const col=blend(CC[D.cats[r]]||'#888', CC[D.cats[c]]||'#888');
  document.getElementById('info').innerHTML = v
    ? `<strong style="color:${col}">${rn} &harr; ${cn}</strong><br>${D.descs[r]}<br><span style="font-size:10.5px;color:var(--color-text-tertiary)">${D.cats[r]} &middot; ${D.cats[c]}</span><br>${D.descs[c]}`
    : `<strong>${rn}</strong> and <strong>${cn}</strong> &mdash; no direct connection in this build.`;
}
function onHov(r,c,conn){
  document.getElementById('ttip').innerHTML=
    `<b>${D.names[r]} &harr; ${D.names[c]}</b>`+
    (conn?`<span style="color:var(--color-text-success)">&#10003; connected</span>`
         :`<span style="color:var(--color-text-tertiary)">&#10007; not connected</span>`);
}
function clearTip(){
  document.getElementById('ttip').innerHTML='Hover a part label to highlight its row &mdash; click a cell to inspect the connection.';
}
function renderDeg(vis){
  const ds=vis.map(i=>({i,d:deg(i),c:CC[D.cats[i]]||'#888',n:D.names[i]}));
  const mx=Math.max(...ds.map(d=>d.d),1);
  document.getElementById('degbar').innerHTML=ds.map(({i,d,c,n})=>{
    const h=Math.max(3,Math.round(54*(d/mx)));
    return`<div class="db-wrap" onclick="document.getElementById('info').innerHTML='<strong>${n}</strong> &mdash; degree ${d}. ${D.descs[i]}'">
      <div class="db" style="height:${h}px;background:${c}" title="${n}: ${d}"></div>
      <div class="dlb">${d}</div>
    </div>`;
  }).join('');
}

render();
</script>
)---"

js_str_esc <- function(x) {            # escape an R string for a JS string literal
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub('"', '\\\\"', x)
  x <- gsub("\n", " ", x); x <- gsub("\r", " ", x)
  x
}
js_arr_lit <- function(x) paste0("[", paste0('"', js_str_esc(x), '"', collapse = ","), "]")

write_adjacency_html <- function(A_inst, node, part_num, fam_v, name_v, crit,
                                 out_prefix, FAM_COL,
                                 title = NULL, theme = NULL, source_label = "MPD/LDR parsed") {
  # --- collapse instance matrix -> unique part-TYPE matrix -------------------
  types  <- unique(part_num)
  nt     <- length(types)
  tfam   <- vapply(types, function(t) fam_v[match(t, part_num)], "")
  tname  <- vapply(types, function(t) { nm <- name_v[match(t, part_num)]
                                        if (is.na(nm) || nm == "") t else nm }, "")
  tcount <- as.integer(table(factor(part_num, levels = types)))
  ti     <- match(part_num, types)                      # instance -> type index

  M <- matrix(0L, nt, nt)
  nz <- which(A_inst == 1L, arr.ind = TRUE)
  for (r in seq_len(nrow(nz))) {
    a <- ti[nz[r, 1]]; b <- ti[nz[r, 2]]
    if (a != b) { M[a, b] <- 1L; M[b, a] <- 1L }
  }
  diag(M) <- 0L
  tdeg <- rowSums(M)

  # --- per-type description (family + usage count + connectivity note) -------
  q85 <- stats::quantile(tdeg, 0.85)
  tdesc <- vapply(seq_len(nt), function(i) {
    hub <- if (tdeg[i] == 0) "isolated (no in-build connection detected)"
           else if (tdeg[i] >= q85) "a highly connected hub of the build"
           else "connects to adjacent parts in the assembly"
    sprintf("%s &middot; family %s &middot; used &times;%d &middot; %s.",
            tname[i], tfam[i], tcount[i], hub)
  }, "")

  # --- order types by family then part number (matches the heatmap order) ----
  ord   <- order(match(tfam, names(FAM_COL)), types)
  types <- types[ord]; tfam <- tfam[ord]; tname <- tname[ord]
  tcount <- tcount[ord]; tdesc <- tdesc[ord]; M <- M[ord, ord, drop = FALSE]
  tdeg  <- tdeg[ord]

  # --- JS data object --------------------------------------------------------
  mat_rows <- apply(M, 1, function(r) paste0("[", paste(r, collapse = ","), "]"))
  data_js <- paste0(
    "{\nparts:", js_arr_lit(paste0(types, ".dat")),
    ",\nnames:", js_arr_lit(tname),
    ",\ncats:",  js_arr_lit(tfam),
    ",\ndescs:", js_arr_lit(tdesc),
    ",\nmatrix:[", paste(mat_rows, collapse = ","), "]\n}")

  # --- CC colour map (only the families present) -----------------------------
  fams_present <- intersect(names(FAM_COL), unique(tfam))
  cc_js <- paste0("{",
                  paste0(fams_present, ":'", FAM_COL[fams_present], "'", collapse = ","),
                  ",other:'#BAB0AC'}")

  # --- legend swatches -------------------------------------------------------
  legend_html <- paste(vapply(fams_present, function(f)
    sprintf('  <span><span class="ld" style="background:%s"></span>%s</span>',
            FAM_COL[f], f), ""), collapse = "\n")

  # --- header / stat fields --------------------------------------------------
  n_conn <- sum(M) / 2
  n_iso  <- sum(tdeg == 0)
  n_inst <- length(part_num)
  if (is.null(title)) title <- out_prefix
  if (is.null(theme)) theme <- sprintf("%d part types", nt)
  srtitle <- sprintf("%s &mdash; adjacency matrix of %d part types and %d connections",
                     title, nt, n_conn)

  html <- HTML_TEMPLATE
  html <- gsub("__DATA__",   data_js, html, fixed = TRUE)
  html <- gsub("__CC__",     cc_js,   html, fixed = TRUE)
  html <- gsub("__LEGEND__", legend_html, html, fixed = TRUE)
  html <- gsub("__SRTITLE__", srtitle, html, fixed = TRUE)
  html <- gsub("__TITLE__",  title,   html, fixed = TRUE)
  html <- gsub("__THEME__",  theme,   html, fixed = TRUE)
  html <- gsub("__SOURCE__", source_label, html, fixed = TRUE)
  html <- gsub("__NPARTS__", as.character(nt),     html, fixed = TRUE)
  html <- gsub("__NCONN__",  as.character(n_conn), html, fixed = TRUE)
  html <- gsub("__NINST__",  as.character(n_inst), html, fixed = TRUE)
  html <- gsub("__NISO__",   as.character(n_iso),  html, fixed = TRUE)

  out <- paste0(out_prefix, "_adjacency.html")
  writeLines(html, out, useBytes = TRUE)
  out
}

# --- 7. Driver --------------------------------------------------------------
# --- interactive sub-model chooser (used when ask = TRUE) -------------------
.prompt_exclusions <- function(gtab, preselected) {
  cat("\nSub-models in this set (a 'part' is one placed instance):\n")
  for (i in seq_len(nrow(gtab))) {
    mk <- if (gtab$group[i] %in% preselected) "  [auto-excluded: minifig]" else ""
    cat(sprintf("  %2d. %-48s %4d parts%s\n", i, gtab$group[i], gtab$n[i], mk))
  }
  cat("\nEnter the numbers of ADDITIONAL sub-models to exclude\n")
  cat("(comma/space separated; blank = keep all the rest): ")
  ans <- trimws(readline())
  if (!nzchar(ans)) return(character(0))
  idx <- suppressWarnings(as.integer(strsplit(ans, "[ ,]+")[[1]]))
  idx <- idx[!is.na(idx) & idx >= 1 & idx <= nrow(gtab)]
  gtab$group[idx]
}

run_lego_adjacency <- function(mpd_path, rebrickable_dir = NULL, out_prefix = NULL,
                               TOL = 10, exclude = NULL,
                               exclude_minifigs = FALSE, ask = FALSE) {
  suppressPackageStartupMessages({
    library(igraph); library(ggplot2); library(ggrepel)
    library(reshape2); library(data.table)
  })
  stopifnot(file.exists(mpd_path))
  if (is.null(out_prefix)) out_prefix <- sub("\\.[^.]*$", "", basename(mpd_path))

  subfiles <- parse_mpd(mpd_path)
  raw      <- expand_model(subfiles)
  if (length(raw) == 0) stop("No placed parts found in ", mpd_path)

  meta <- load_part_meta(rebrickable_dir)
  for (k in seq_along(raw)) {
    m   <- resolve_meta(raw[[k]]$dat, meta)
    fam <- family_of(m$name, m$cat)
    raw[[k]]$name <- if (is.na(m$name)) raw[[k]]$dat else m$name
    raw[[k]]$cat  <- m$cat
    raw[[k]]$fam  <- fam
    raw[[k]]$dims <- dims_of(m$name, fam)
  }

  # --- exclusions: drop whole minifig / ancillary sub-models -----------------
  # Each part carries $group = the named .mpd sub-model it descends from.
  # Exclusion is GROUP-based (a minifigure is a sub-model, not a loose part),
  # so an excluded figure takes its accessories (blasters, tools) with it.
  grp_v   <- vapply(raw, function(p) if (is.null(p$group)) "(root)" else p$group, "")
  fam_all <- vapply(raw, `[[`, "", "fam")
  gtab <- data.frame(group = names(table(grp_v)),
                     n     = as.integer(table(grp_v)),
                     stringsAsFactors = FALSE)
  # A sub-model is a "minifig" if its name says so, or if most of its parts
  # are minifig-family (catches figures whose sub-model name is generic).
  name_is_mf <- grepl("minifig|figure|droid", gtab$group, ignore.case = TRUE)
  frac_mf    <- vapply(gtab$group, function(g) mean(fam_all[grp_v == g] == "minifig"), 0)
  minifig_groups <- gtab$group[name_is_mf | frac_mf >= 0.5]

  if (ask) {
    auto <- if (exclude_minifigs) minifig_groups else character(0)
    exclude <- unique(c(exclude, auto, .prompt_exclusions(gtab, auto)))
    exclude_minifigs <- FALSE   # already folded any auto picks into `exclude`
  }

  drop_groups <- character(0)
  if (exclude_minifigs) drop_groups <- c(drop_groups, minifig_groups)
  if (length(exclude))  drop_groups <- c(drop_groups, exclude)
  drop_groups <- unique(drop_groups)
  drop <- tolower(trimws(grp_v)) %in% tolower(trimws(drop_groups))
  if (any(drop)) {
    cat(sprintf("Excluded         : %d parts in %d sub-model(s)  (%s)\n",
                sum(drop), length(unique(grp_v[drop])),
                paste(sort(unique(grp_v[drop])), collapse = ", ")))
    raw <- raw[!drop]
    if (length(raw) == 0) stop("All parts were excluded — nothing to analyse.")
  }

  dat_v  <- vapply(raw, `[[`, "", "dat")
  idx    <- ave(seq_along(dat_v), dat_v, FUN = seq_along)
  node   <- paste0(dat_v, "#", idx)
  part_num <- dat_v
  fam_v  <- vapply(raw, `[[`, "", "fam")
  name_v <- vapply(raw, `[[`, "", "name")

  ad   <- build_adjacency(raw, tol = TOL)
  A    <- ad$A; crit <- ad$crit
  dimnames(A) <- list(node, node)
  deg  <- rowSums(A)

  cat(sprintf("Model            : %s\n", out_prefix))
  cat(sprintf("Part instances   : %d\n", length(raw)))
  cat(sprintf("Connections      : %d\n", sum(A)/2))
  cat(sprintf("Isolated parts   : %d  (%s)\n", sum(deg==0),
              paste(node[deg==0], collapse=", ")))
  cat(sprintf("Families         : %s\n", paste(sort(unique(fam_v)), collapse=", ")))

  FAM_COL <- c(brick="#4E79A7", plate="#59A14F", tile="#8CD17D", slope="#B07AA1",
               round="#499894", cone="#86BCB6", baseplate="#A0CBE8",
               panel="#F1CE63", bracket="#B6992D", technic="#E15759",
               wheel="#79706E", wheel_holder="#D37295", tyre="#4B4B4B",
               minifig="#FF9D9A", other="#BAB0AC")
  fam_col_v <- FAM_COL[fam_v]; fam_col_v[is.na(fam_col_v)] <- FAM_COL["other"]

  # (A) adjacency heatmap, instances grouped by family
  ord   <- order(match(fam_v, names(FAM_COL)), part_num)
  As    <- A[ord, ord]
  labs  <- part_num[ord]
  rownames(As) <- colnames(As) <- node[ord]
  hm <- reshape2::melt(As, varnames = c("A","B"), value.name = "v")
  hm$Ai <- match(hm$A, node[ord]); hm$Bi <- match(hm$B, node[ord])
  axis_col <- fam_col_v[ord]
  p1 <- ggplot(hm, aes(Bi, Ai, fill = factor(v))) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_manual(values = c("0"="#F4F4F2","1"="#2C6E8F"),
                      labels = c("not connected","connected"), name = NULL) +
    scale_x_continuous(breaks = seq_along(labs), labels = labs, expand = c(0,0)) +
    scale_y_reverse(breaks = seq_along(labs), labels = labs, expand = c(0,0)) +
    coord_equal() +
    labs(title = sprintf("%s - part adjacency matrix", out_prefix),
         subtitle = sprintf("%d instances | %d connections | proximity AND compatibility | grouped by family",
                            length(raw), sum(A)/2), x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 6, colour = axis_col),
          axis.text.y = element_text(size = 6, colour = axis_col),
          panel.grid = element_blank(), legend.position = "top")
  ggsave(paste0(out_prefix, "_adjacency_matrix.png"), p1,
         width = max(7, 0.32*length(raw)), height = max(6.5, 0.32*length(raw)),
         dpi = 200, limitsize = FALSE)

  # (B) network with ggrepel labels, node colour = family
  g <- graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE)
  set.seed(42); lay <- layout_with_fr(g)
  nd <- data.frame(x = lay[,1], y = lay[,2], part = part_num, fam = fam_v)
  ee <- as.data.frame(as_edgelist(g, names = FALSE))
  ed <- data.frame(x = lay[ee[,1],1], y = lay[ee[,1],2],
                   xend = lay[ee[,2],1], yend = lay[ee[,2],2])
  p2 <- ggplot() +
    geom_segment(data = ed, aes(x, y, xend = xend, yend = yend),
                 colour = "#CCCCCC", linewidth = 0.5) +
    geom_point(data = nd, aes(x, y, fill = fam), shape = 21, size = 5,
               colour = "white", stroke = 0.8) +
    ggrepel::geom_text_repel(data = nd, aes(x, y, label = part),
                             size = 2.4, max.overlaps = Inf, seed = 42,
                             segment.size = 0.2, segment.colour = "#999999",
                             min.segment.length = 0, box.padding = 0.3) +
    scale_fill_manual(values = FAM_COL, name = "part family") +
    coord_equal() +
    labs(title = sprintf("%s - part connectivity network", out_prefix),
         subtitle = "node = part instance | label = part number | colour = family") +
    theme_void(base_size = 10) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey40"))
  ggsave(paste0(out_prefix, "_network.png"), p2, width = 10, height = 8, dpi = 200)

  # (D) network on REAL model coordinates (top view: x vs z), same family style
  cx <- vapply(raw, `[[`, 0, "x"); cz <- vapply(raw, `[[`, 0, "z")
  ndc <- data.frame(x = cx, z = cz, part = part_num, fam = fam_v)
  eec <- as.data.frame(as_edgelist(g, names = FALSE))
  edc <- if (nrow(eec)) data.frame(x = cx[eec[,1]], z = cz[eec[,1]],
                                   xend = cx[eec[,2]], zend = cz[eec[,2]]) else
         data.frame(x=numeric(), z=numeric(), xend=numeric(), zend=numeric())
  p3 <- ggplot() +
    geom_segment(data = edc, aes(x, z, xend = xend, yend = zend),
                 colour = "#CCCCCC", linewidth = 0.5) +
    geom_point(data = ndc, aes(x, z, fill = fam), shape = 21, size = 5,
               colour = "white", stroke = 0.8) +
    ggrepel::geom_text_repel(data = ndc, aes(x, z, label = part),
                             size = 2.4, max.overlaps = Inf, seed = 42,
                             segment.size = 0.2, segment.colour = "#999999",
                             min.segment.length = 0, box.padding = 0.3) +
    scale_fill_manual(values = FAM_COL, name = "part family") +
    scale_y_reverse() + coord_equal() +
    labs(title = sprintf("%s - part connectivity network (real coordinates)", out_prefix),
         subtitle = "nodes at real model coordinates (top view) | label = part number | colour = family",
         x = "x (LDU)", y = "z (LDU)") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey40"),
          panel.grid.minor = element_blank())
  ggsave(paste0(out_prefix, "_network_coords.png"), p3, width = 10, height = 8, dpi = 200)

  # (E) interactive HTML adjacency widget (type-level, self-contained)
  html_out <- write_adjacency_html(
    A_inst = A, node = node, part_num = part_num, fam_v = fam_v,
    name_v = name_v, crit = crit, out_prefix = out_prefix, FAM_COL = FAM_COL,
    title = out_prefix, theme = sprintf("%d part types", length(unique(part_num))),
    source_label = "MPD/LDR parsed")

  # (C) exports
  write.csv(A, paste0(out_prefix, "_adjacency_matrix.csv"))
  N <- length(raw); el <- list()
  for (i in seq_len(N)) for (j in seq_len(N)) if (j > i && A[i,j] == 1L)
    el[[length(el)+1]] <- data.frame(
      part_A = node[i], num_A = part_num[i], fam_A = fam_v[i], name_A = name_v[i],
      part_B = node[j], num_B = part_num[j], fam_B = fam_v[j], name_B = name_v[j],
      criterion = crit[i,j], stringsAsFactors = FALSE)
  edges <- if (length(el)) do.call(rbind, el) else
           data.frame(part_A=character(),num_A=character(),fam_A=character(),name_A=character(),
                      part_B=character(),num_B=character(),fam_B=character(),name_B=character(),
                      criterion=character())
  write.csv(edges, paste0(out_prefix, "_edges.csv"), row.names = FALSE)

  cat(sprintf("Wrote: %s_{adjacency_matrix.csv, edges.csv, adjacency_matrix.png, network.png, network_coords.png, adjacency.html}\n",
              out_prefix))
  invisible(list(A = A, edges = edges, parts = raw, html = html_out))
}

# --- Renamed variant: adjacency of the MODEL ONLY --------------------------
# Same pipeline as run_lego_adjacency(), but built for analysing the assembled
# model without its figures/accessories. By default it auto-drops every
# minifig-family part and, when run interactively (ask = TRUE, the default),
# prints a numbered menu of the set's named sub-models so you can also exclude
# ancillary structures (flagpoles, stands, spare parts, etc.). Pass
# ask = FALSE for non-interactive/batch use, optionally with an explicit
# `exclude =` vector of sub-model names.
run_lego_model_adjacency <- function(mpd_path, rebrickable_dir = NULL,
                                     out_prefix = NULL, TOL = 10,
                                     exclude = NULL, exclude_minifigs = TRUE,
                                     ask = interactive()) {
  run_lego_adjacency(mpd_path, rebrickable_dir = rebrickable_dir,
                     out_prefix = out_prefix, TOL = TOL,
                     exclude = exclude, exclude_minifigs = exclude_minifigs,
                     ask = ask)
}
