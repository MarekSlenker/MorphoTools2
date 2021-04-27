context("plotPoints.cdadata_hist")

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

test_that("2 skupiny",  {

  cdaRes = cda.calc(morphoMockup)

  expect_is(cdaRes, "cdadata")

  expect_warning(plotPoints(cdaRes, axes = c(1,3)), "The object has only one axis, which will be plotted")

  expect_warning(plotPoints(cdaRes, axes = 4), "The object has only one axis, which will be plotted")

  expect_warning(plotPoints(cdaRes, labels = T), "Labels = TRUE is not supported for histograms.")
})




test_that("2+ skupiny pasivne",  {

  cdaRes = cda.calc(centaurea, passiveSamples = c("hybr", "ph"))

  expect_warning(plotPoints(cdaRes, axes = c(1,3)), "The object has only one axis, which will be plotted")

  expect_warning(plotPoints(cdaRes, axes = 4), "The object has only one axis, which will be plotted")

  expect_warning(plotPoints(cdaRes, labels = T), "Labels = TRUE is not supported for histograms.")

})


