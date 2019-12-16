setwd("/home/mint/Dropbox/Git/MorphoTools")

library(devtools)
library(roxygen2)

devtools::load_all()

devtools::document()

usethis::use_r("summary.morphodata.R")

read.mo

devtools::test()

library(testthat)

BiocManager::install("testthat", lib = "/home/mint/R/x86_64-pc-linux-gnu-library/3.6")

is.nan(dd$data)
