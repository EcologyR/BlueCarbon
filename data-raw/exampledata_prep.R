DataInv <- read.csv("data-raw/bluecarbon_data.csv", sep= ";")


usethis::use_data(DataInv, overwrite = TRUE)
