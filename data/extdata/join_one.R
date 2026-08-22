# Joins:
# 0 inventory_parts
# 1 . , inventories
# 2 . , parts
# 3 . , part_categories
# 4 . , colors
# 5 . , sets
# 6 . , themes

library("dplyr")
lego_parts <- left_join(inventory_parts, inventories,
  by=c("inventory_id" = "id")) %>%
  left_join(parts, by="part_num") %>%
  left_join(part_categories, by=c("part_cat_id"="id")) %>%
  rename("part_name"=name.x, "part_category"=name.y) %>%
  left_join(colors, by=c("color_id"="id")) %>%
  rename(color_name="name") %>%
  left_join(sets, by="set_num") %>%
  left_join(themes, by=c("theme_id" = "id")) %>%
  rename("set_name"=name.x, "theme"=name.y)

lego_parts <- rename(lego_parts, "set_img_url"=img_url.y, "part_img_url"=img_url.x)


lego_parts<- lego_parts %>% select(theme, 
                      theme_id, 
                      set_img_url,
                      set_num, 
                      set_name, 
                      year, 
                      num_parts,
                      inventory_id, 
                      part_cat_id,
                      part_category,
                      part_num, 
                      part_name,
                      quantity,
                      part_img_url,
                      part_material,
                      color_id,
                      color_name,
                      is_trans,
                      rgb,
                      parent_id,
                      is_spare,
                      version)
write_csv(lego_parts, "data/extdata/lego_parts.csv")

lego_sets <- left_join(sets, themes, by=c("theme_id" = "id"))
lego_sets <- rename(lego_sets, "set_name"=name.x, "theme_name"=name.y)
    



