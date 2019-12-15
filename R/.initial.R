setwd("/home/mint/Dropbox/Git/MorphoTools")

library(devtools)
library(roxygen2)

devtools::load_all()

roxygenise()

usethis::use_r("summary.morphodata.R")

