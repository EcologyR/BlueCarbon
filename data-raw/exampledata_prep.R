bluecarbon_data <- read.csv("data-raw/bluecarbon_data.csv", sep= ";")


usethis::use_data(bluecarbon_data, overwrite = TRUE)
