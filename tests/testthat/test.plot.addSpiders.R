context("plotAddSpiders")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "data" = data.frame(
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18)))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)


options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)

test_that("plotAddSpiders error input",  {

  cdaRes = cda.calc(centaurea)

  plotPoints(cdaRes, pch =c(18,16), col = c("red", "green", "blue", "black", "yellow"), cex = 0.5)

  expect_error(plotAddSpiders(cdaRes, axes = 1), "you have to specifi 2 axes (e.g., axes = c(1,2))", fixed = TRUE)
  expect_error(plotAddSpiders(cdaRes, axes = c(1,5)), "specified axes are out of bounds. Object has only 3 axes.")

  cdaRes = cda.calc(morphoMockup)
  expect_error(plotAddSpiders(cdaRes), "The method plotAddSpiders() is not applicable to histogram.", fixed = TRUE)

  pcaRes = pca.calc(centaurea)
  expect_error(plotAddEllipses(pcaRes, axes = 1), "you have to specifi 2 axes (e.g., axes = c(1,2))", fixed = TRUE)
  expect_error(plotAddEllipses(pcaRes, axes = c(1,45)), "specified axes are out of bounds. Object has only 25 axes.")
})



