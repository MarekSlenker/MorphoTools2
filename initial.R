setwd("/home/mint/Dropbox/Git/MorphoTools")

library(devtools)
library(roxygen2)

devtools::load_all()

devtools::document()




Boptions(warn=1)

usethis::use_r("newMorphodata")


devtools::test()


is.nan(dd$data)
sample_decComa

export.res(object)
