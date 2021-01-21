context("plotAddLabels.characters_hist")

options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)

cdaRes = cda.calc(centaurea, passiveSamples = c("hybr", "ph"))

test_that("cda wrong input",  {

  plotCharacters(cdaRes, labels = F)

  expect_warning(plotAddLabels.characters(cdaRes, axes = 2), "The object has only one axis, which will be plotted", fixed = TRUE)


  expect_warning(plotAddLabels.characters(cdaRes, axes = c(2,26)), "The object has only one axis, which will be plotted")

  expect_error(plotAddLabels.characters(cdaRes, labels = "eeee", pos = 4, cex = 1), "label eeee does not exist")

  expect_error(plotAddLabels.characters(cdaRes, include = F), "No labels to plot. You specified to exclude (include = FALSE) all labels", fixed = TRUE)

})
