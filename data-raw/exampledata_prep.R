core_comp <- read.csv("data-raw/core_comp.csv", sep= ";")


usethis::use_data(core_comp, overwrite = TRUE)
