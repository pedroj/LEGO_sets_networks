library(dplyr)

target_set <- "911721-1"    # unique(Starwars$set_num)  # choose your set_num values

url_vec <- sets %>%  
    dplyr::filter(set_num %in% target_set) %>%
    dplyr::select(set_num, img_url) %>%
    tibble::deframe()   # named character vector: names = set_num, values = img_url

# 2. Download and read into a list of images
# Assuming JPEGs (Rebrickable uses jpg):

library(jpeg)   # install.packages("jpeg") if needed

my_image <- lapply(names(url_vec), 
                     function(sn) {
    tmp <- tempfile(fileext = ".jpg")
    download.file(url_vec[sn], tmp, mode = "wb")
    readJPEG(tmp)
})

names(image_list) <- names(url_vec)

# Now image_list[["001-1"]] is the image matrix for a set.

# 3. Plot one image by set_num
library(grid)
plot_set_image <- function(sn) {
    img <- image_list[[sn]]
    grid::grid.raster(img)
}
plot_set_image(target_set)

