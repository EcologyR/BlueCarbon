exampledata <- read.csv("data-raw/example.csv", sep= ";")


usethis::use_data(exampledata, overwrite = TRUE)
