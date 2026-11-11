# classify_part.R - part category / subcategory classifier
# Copied from the LEGO project's plot_lego_network.R (same rules, so categories
# match the network plots and the compatibility matrix).
# classify_part(name, rebrickable_category) -> c(category = , subtype = )

classify_part <- function(name, category = "") {
  n <- tolower(gsub("\\s+", " ", paste(name)))
  ct <- tolower(paste(category))
  has <- function(p, x = n) grepl(p, x, perl = TRUE)
  out <- function(c, s) c(category = c, subtype = s)
  if (has("^[~=_]")) n <- sub("^[~=_|]+", "", n)

  # Minifig and accessories
  if (has("\\bminifig") || has("^minifig", ct)) {
    if (has("\\bhead\\b(?!gear|dress|band)") && !has("hair|hat|helmet|cap\\b|headgear|crown|hood"))
      return(out("Minifig", "Minifig head"))
    if (has("torso")) return(out("Minifig", "Minifig torso (with arms)"))
    if (has("\\bhand\\b")) return(out("Minifig", "Minifig hand"))
    if (has("hips|legs?\\b")) return(out("Minifig", "Minifig hips and legs"))
    if (has("hair|hat|helmet|\\bcap\\b|headgear|headdress|crown|hood|headwear") || has("headwear", ct))
      return(out("Minifig", "Minifig headgear or hair"))
    if (has("neck|backpack|cape|armou?r|vest|airtanks")) return(out("Minifig", "Minifig neckwear"))
    return(out("Minifig", "Minifig accessory (tool; weapon)"))
  }
  # Wheels
  if (has("\\bty(re|res)\\b|\\btire")) return(out("Special", "Tyre"))
  if (has("(brick|plate).*wheel|wheels? holder") && !has("^bar\\b"))
    return(out("Special", "Wheel holder"))
  if (has("\\bwheel|\\brim\\b")) return(out("Special", "Wheel rim"))
  # Technic
  if (has("technic") || has("technic", ct)) {
    if (has("\\bbrick\\b")) return(out("Technic", "Technic brick (studded beam)"))
    if (has("\\bplate\\b")) return(out("Technic", "Technic plate (with holes)"))
    if (has("\\bpin\\b") && has("\\bbar\\b")) return(out("Technic", "Technic pin with bar"))
    if (has("\\bpin\\b") && has("ball")) return(out("Technic", "Technic pin with ball"))
    if (has("axle pin|pin.*axle|axle.*pin")) return(out("Technic", "Technic pin with axle"))
    if (has("\\bpin\\b") && !has("connector|joiner|hole")) return(out("Technic", "Technic pin"))
    if (has("\\bbush")) return(out("Technic", "Technic bush"))
    if (has("gear|worm|\\brack\\b|differential|turntable")) return(out("Technic", "Technic gear"))
    if (has("chain|tread|track")) return(out("Technic", "Technic chain or tread link"))
    if (has("connector|joiner|block|cross|angle|hub|steering|shock|link")) return(out("Technic", "Technic connector (cross block; angle)"))
    if (has("\\baxle\\b")) return(out("Technic", "Technic axle"))
    if (has("panel|fairing")) return(out("Technic", "Technic panel or fairing"))
    if (has("\\bbrick\\b")) return(out("Technic", "Technic brick (studded beam)"))
    if (has("\\bplate\\b")) return(out("Technic", "Technic plate (with holes)"))
    if (has("beam|liftarm")) return(out("Technic", "Technic beam (liftarm)"))
    return(out("Technic", "Technic connector (cross block; angle)"))
  }
  # Special connection parts
  if (has("hinge")) return(out("Special", "Click hinge (plate or brick)"))
  if (has("turntable")) return(out("Special", "Turntable"))
  if (has("socket|ball cup")) return(out("Special", "Ball-joint plate (socket)"))
  if (has("tow ball|ball joint|with ball|\\bball\\b")) return(out("Special", "Ball-joint plate (ball)"))
  if (has("bracket")) return(out("Special", "Bracket (SNOT)"))
  if (has("\\bclip")) return(out("Special", "Clip plate or tile"))
  if (has("handle|bar holder|with bar\\b")) return(out("Special", "Bar holder or handle plate"))
  if (has("\\bdish\\b|radar")) return(out("Special", "Dish or radar"))
  if (has("\\bflag")) return(out("Special", "Flag"))
  if (has("hose|flex|\\btube\\b|\\bstring")) return(out("Special", "Hose or flex bar"))
  if (has("plant|leaves|flower|stem|grass|\\btree|vine|foliage")) return(out("Special", "Plant or foliage"))
  if (has("^(bar|antenna)|\\bbar \\d|antenna|lightsaber|whip|\\brod\\b")) return(out("Special", "Bar or antenna"))
  # Structural families
  if (has("baseplate")) return(out("Baseplate", "Baseplate"))
  if (has("windscreen|windshield|canopy")) return(out("Panel", "Windscreen"))
  if (has("glass|pane\\b|shutter|door\\b(?! ?frame)|\\bgate\\b"))
    return(out("Panel", "Window or door leaf"))
  if (has("window|door ?frame|\\bframe\\b")) return(out("Panel", "Window frame"))
  if (has("\\bpanel\\b|\\bwall\\b|\\bfence\\b")) return(out("Panel", "Panel"))
  if (has("^tile|\\btile\\b") || has("^tiles", ct)) {
    if (has("hollow bar|with bar|open stud|hollow stud")) return(out("Tile", "Tile with bar or open stud"))
    if (has("round|circle|quarter")) return(out("Tile", "Tile round"))
    if (has("grille|slope|modified|curved|corner")) return(out("Tile", "Tile modified (grille; slope tile)"))
    return(out("Tile", "Tile basic"))
  }
  if (has("slope|curved|\\bcurve\\b") || has("slopes", ct)) {
    if (has("inverted")) return(out("Slope", "Slope inverted"))
    if (has("curved|curve")) return(out("Slope", "Slope curved"))
    return(out("Slope", "Slope standard"))
  }
  if (has("\\bcone\\b")) return(out("Brick", "Cone"))
  if (has("\\barch\\b")) return(out("Brick", "Brick arch"))
  if (has("\\bwedge\\b") && has("plate")) return(out("Plate", "Plate wedge"))
  if (has("\\bwedge\\b")) return(out("Slope", "Slope standard"))
  if (has("^plate|\\bplate\\b") || has("^plates", ct)) {
    if (has("pin hole|with hole|axle hole") && !has("round")) return(out("Plate", "Plate with pin hole"))
    if (has("jumper|with 1 stud|1 x 2 with groove|centre stud|center stud"))
      return(out("Plate", "Plate jumper"))
    if (has("round|circle")) return(out("Plate", "Plate round"))
    return(out("Plate", "Plate basic"))
  }
  if (has("^brick|\\bbrick\\b") || has("^bricks", ct)) {
    if (has("round|cylinder")) return(out("Brick", "Brick round"))
    if (has("with (pin|axle)|with technic")) return(out("Brick", "Brick with pin or axle"))
    if (has("studs? on (\\d+ )?(side|sides)|headlight|with side studs?|snot")) return(out("Brick", "Brick modified SNOT"))
    return(out("Brick", "Brick basic"))
  }
  out("Other", "Other")
}

# Vectorised wrapper: returns a data.frame with columns category, subcategory
classify_parts <- function(names, rb_categories) {
  res <- mapply(classify_part, names, rb_categories, USE.NAMES = FALSE)
  data.frame(category = res[1, ], subcategory = res[2, ], stringsAsFactors = FALSE)
}
