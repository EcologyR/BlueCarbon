bluecarbon_data <- read.csv("data-raw/bluecarbon_data.csv", sep= ";")
names(bluecarbon_data)[5] <- "compaction"

usethis::use_data(bluecarbon_data, overwrite = TRUE)


core_comp <- read.csv("data-raw/core_comp.csv", sep= ";")


usethis::use_data(core_comp, overwrite = TRUE)
