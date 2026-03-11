# global.R
library(dplyr)

# Direct download from Rebrickable.
urlthemes <- "https://cdn.rebrickable.com/media/downloads/themes.csv.gz"
urlparts<- "https://cdn.rebrickable.com/media/downloads/parts.csv.gz"
urlsets<- "https://cdn.rebrickable.com/media/downloads/sets.csv.gz?1678349282.4992967"

##----------------------------
dwld <- function(url) {
    tmp <- tempfile()
    download.file(url,tmp)  
    read.csv(
        gzfile(tmp),
        sep=",",
        header=TRUE,
        stringsAsFactors=T)
    #names(data)[1] <- sub("X\\.","",names(data)[1])
}
#-----------------------------

set_parts <- dwld(urlparts)
lego_sets <- dwld(urlsets)
themes <- dwld(urlthemes)


lego_sets <- readRDS("lego_sets.rds")
set_parts <- readRDS("set_parts.rds")

# ✔ Successfully deployed to <https://z2leso-pedro-jordano.shinyapps.io/lego-set-explorer/>
# 