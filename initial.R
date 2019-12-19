setwd("/home/mint/Dropbox/Git/MorphoTools")

library(devtools)
library(roxygen2)

devtools::load_all()

devtools::document()

usethis::use_r("summary.morphodata.R")


devtools::test()


is.nan(dd$data)
sample_decComa

