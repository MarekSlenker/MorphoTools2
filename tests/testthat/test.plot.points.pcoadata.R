context("plotPoints.pcoadata")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "Ch1" = c(1,3,4,6,1,7,12,8),
                             "Ch2" = c(11, 12,42,12,32,11,22,18))

morphoMockup = .morphodataFromDataFrame(morphoDataFrame)

# locally suppress warnings
options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)


test_that("ploting with error parameters",  {

  pcoaRes = pcoa.calc(morphoMockup)

  expect_is(pcoaRes, "pcoadata")

  expect_error(plotPoints(pcoaRes, axes = c(3,33)), "Specified axes are out of bounds. Object has only 5 axes." )

  expect_error(plotPoints(pcoaRes, axes = c(1,1,2))) # "you have to specifi 2 axes (e.g., axes = c(1,2))"
})
