dim(colors)
names(colors)
## [1] "id"       "name"     "rgb"      "is_trans"
#
dim(inventories)
names(inventories)
## [1] "id"      "version" "set_num"
#
dim(inventory_parts)
names(inventory_parts)
## [1] "inventory_id" "part_num"     "color_id"     "quantity"     "is_spare"
#
dim(inventory_sets)
names(inventory_sets)
## [1] "inventory_id" "set_num"      "quantity"
#
dim(part_categories)
names(part_categories)
## [1] "id"   "name"
#
dim(parts)
names(parts)
## [1] "part_num"    "name"        "part_cat_id"
#
dim(sets)
names(sets)
## [1] "set_num"   "name"      "year"      "theme_id"  "num_parts"
#
dim(themes)
names(themes)
## [1] "id"        "name"      "parent_id"
