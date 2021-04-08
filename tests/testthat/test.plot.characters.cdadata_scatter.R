context("plotCharacters.cdadata_scatter")



# locally suppress warnings
options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)

cdaRes = cda.calc(centaurea)

test_that("ploting with error parameters",  {

  expect_error(plotCharacters(cdaRes, axes = c(3,5)), "specified axes are out of bounds. Object has only 3 axes.", fixed = TRUE)

  expect_error(plotCharacters(cdaRes, axes = c(1,1,2)), "you have to specify 2 axes (e.g., axes = c(1,2))", fixed = TRUE) #
})

