# Same interface / mating-rule / subtype tables as lego_theoretical_compatibility.R (project doc),
# plus ONE added subtype: "Minifig seat or chair" (S|A), needed for part 4079.
# Writes only the subtype x subtype tables (no plots): compat_matrix_subtype_codes.csv etc.
out_dir <- Sys.getenv("COMPAT_OUT", "lego_compat_out"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

interfaces <- read.csv(text = "
code,interface,description
S,stud,Solid or hollow stud on top or on a side (SNOT)
HS,hollow stud,Open stud; also grips a 3.18 mm bar
A,anti-stud,Underside tubes and walls that clutch a stud
B,bar,3.18 mm bar (handles; antennas; tools; hose ends)
C,clip,C-shaped clip that grips a bar (includes flag clips)
P,Technic pin,Friction or frictionless pin end
PH,pin hole,Round Technic hole
X,axle,Cross axle
XH,axle hole,Cross-shaped hole
BL,ball,Ball joint (towball or small ball)
SK,socket,Ball socket (cup)
HG,click hinge,Interlocking hinge fingers
TT,turntable,Turntable top and base halves
G,gear teeth,Gear teeth that mesh (not a rigid joint)
TR,track link,Chain or tread link
RIM,rim,Wheel rim seat for a tyre
TY,tyre,Tyre bead
DHm,door hinge pin,Pins on a door or gate
DHf,door hinge seat,Pin holes in a door frame
AHm,arm socket,Minifig wrist socket
AHf,hand peg,Minifig hand peg
MJ,hip joint,Minifig torso-hips-legs joint
", strip.white = TRUE)

rules <- read.csv(text = "
a,b,system,certainty,note
S,A,stud,high,Stud into anti-stud clutch (the System connection)
HS,A,stud,high,Hollow studs clutch like solid studs
HS,B,bar,high,Bar inserted in an open stud
A,B,bar,medium,Bar in the small tube of 1-wide bricks and plates only
C,B,clip,high,Clip grips a bar (minifig hands are C)
P,PH,pin,high,Pin in a pin hole
X,XH,axle,high,Axle in an axle hole (locked rotation)
X,PH,axle,high,Axle in a round hole (free rotation)
BL,SK,ball,high,Ball joint
HG,HG,hinge,high,Click-hinge fingers interlock
TT,TT,turntable,high,Turntable halves
G,G,gear,high,Meshing; transmits motion and is not rigid
G,TR,gear,high,Sprocket drives a chain or tread
TR,TR,track,high,Links join into a chain or tread
RIM,TY,wheel,high,Tyre on a rim
DHm,DHf,door,high,Door pins in frame holes
AHm,AHf,minifig,high,Hand in arm
MJ,MJ,minifig,high,Torso-hips-legs assembly
", strip.white = TRUE)

subtypes <- read.csv(text = "
category,subtype,family,interfaces,examples
Brick,Brick basic,brick,S|A,3001 3004 3010
Brick,Brick modified SNOT,brick,S|A,87087 11211 (studs on side)
Brick,Brick with pin or axle,brick,S|A|P,2780-style pins on bricks
Brick,Brick round,round,S|HS|A|XH,3941 3062
Brick,Brick arch,brick,S|A,3659 6182
Brick,Cone,cone,S|HS|A,4589 3942
Plate,Plate basic,plate,S|A,3023 3020 3034
Plate,Plate jumper,plate,S|A,3794 87580
Plate,Plate round,plate,S|HS|A,4073 4032
Plate,Plate wedge,plate,S|A,41769 43722
Tile,Tile basic,tile,A,3069 3068
Tile,Tile round,tile,A,98138 14769
Tile,Tile modified (grille; slope tile),tile,A,2412 3070
Slope,Slope standard,slope,S|A,3039 3040
Slope,Slope inverted,slope,S|A,3665 3660
Slope,Slope curved,slope,A,11477 15068 (smooth top)
Baseplate,Baseplate,baseplate,S,3811 3867
Panel,Panel,panel,S|A,4865 87552
Panel,Window frame,panel,S|A|DHf,60592 60593
Panel,Window or door leaf,panel,DHm,60601 60623
Panel,Windscreen,panel,S|A,2437 6070
Technic,Technic pin,technic,P,2780 6558 4274
Technic,Technic pin with axle,technic,P|X,6562 11214
Technic,Technic pin with ball,technic,P|BL,6628
Technic,Technic axle,technic,X,3705 32073 6587
Technic,Technic beam (liftarm),technic,PH|XH,32524 32278 32140
Technic,Technic brick (studded beam),technic,S|A|PH,3700 3701 3895
Technic,Technic plate (with holes),technic,S|A|PH,3709 3738
Technic,Technic connector (cross block; angle),technic,PH|XH|P,6536 32184 32013
Technic,Technic bush,technic,XH,3713 4265c
Technic,Technic gear,technic,XH|G,3647 3648 32270
Technic,Technic panel or fairing,technic,PH,32527 64391
Technic,Technic chain or tread link,technic,TR,3711 3873
Special,Click hinge (plate or brick),bracket,S|A|HG,44301 44302 30383
Special,Clip plate or tile,bracket,S|A|C,4085 15712 63868
Special,Bar holder or handle plate,bracket,S|A|B,2540 48336 4590b
Special,Ball-joint plate (ball),bracket,S|A|BL,14417 14704
Special,Ball-joint plate (socket),bracket,S|A|SK,14418 47456
Special,Turntable,bracket,S|A|TT,3680c01 61485
Special,Bracket (SNOT),bracket,S|A,99780 99781 44728
Special,Dish or radar,other,A|HS,4740 3960
Special,Bar or antenna,other,B,30374 87994 3957
Special,Flag,other,C,2335 4495
Special,Hose or flex bar,other,B,73590 57719
Special,Plant or foliage,other,S|A|B|C,2423 6255
Special,Wheel holder,wheel_holder,S|A|P,4600 6157 122c01
Special,Wheel rim,wheel,PH|XH|RIM,4624 55981 56904
Special,Tyre,tyre,TY,3641 55982
Minifig,Minifig head,minifig,S|A,3626
Minifig,Minifig torso (with arms),minifig,S|AHm|MJ,973c01
Minifig,Minifig hand,minifig,AHf|C,983
Minifig,Minifig hips and legs,minifig,MJ|A,970c00
Minifig,Minifig headgear or hair,minifig,A,3624 62810
Minifig,Minifig accessory (tool; weapon),minifig,B,4499 58247
Minifig,Minifig neckwear,minifig,S|A,2524 (neck bracket)
Minifig,Minifig seat or chair,minifig,S|A,4079 4083 (ADDED)
Minifig,Minifig arm,minifig,AHf|AHm,3818 3819 (ADDED)
", strip.white = TRUE)

if_list <- setNames(strsplit(subtypes$interfaces, "\\|"), subtypes$subtype)
stopifnot(all(unlist(if_list) %in% interfaces$code), all(c(rules$a, rules$b) %in% interfaces$code))
mate <- function(ia, ib) {
  hit <- (rules$a %in% ia & rules$b %in% ib) | (rules$b %in% ia & rules$a %in% ib)
  rules[hit, , drop = FALSE]
}
n <- nrow(subtypes); st <- subtypes$subtype
cell_code <- matrix("", n, n, dimnames = list(st, st)); cell_bin <- matrix(0L, n, n, dimnames = list(st, st))
for (i in seq_len(n)) for (j in i:n) {
  m <- mate(if_list[[i]], if_list[[j]]); if (nrow(m) == 0) next
  sys <- paste(unique(m$system), collapse = "+")
  cert <- if (all(m$certainty == "medium")) "medium" else "high"
  code <- if (cert == "medium") paste0(sys, "?") else sys
  cell_code[i, j] <- cell_code[j, i] <- code; cell_bin[i, j] <- cell_bin[j, i] <- 1L
}
write.csv(subtypes,  file.path(out_dir, "subtypes_interfaces.csv"), row.names = FALSE)
write.csv(interfaces, file.path(out_dir, "interfaces.csv"), row.names = FALSE)
write.csv(rules,     file.path(out_dir, "mating_rules.csv"), row.names = FALSE)
write.csv(cell_code, file.path(out_dir, "compat_matrix_subtype_codes.csv"))
write.csv(cell_bin,  file.path(out_dir, "compat_matrix_subtype_binary.csv"))
cat("subtypes:", n, "| compatible pairs:", sum(cell_bin[upper.tri(cell_bin, diag = TRUE)]), "\n")
