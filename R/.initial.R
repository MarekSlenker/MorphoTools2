library(devtools)
library(roxygen2)

devtools::load_all()

roxygenise()

usethis::use_r("make")

