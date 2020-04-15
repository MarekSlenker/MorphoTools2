context("clust")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "data" = data.frame(
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18)))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)

test_that("ploting with error parameters",  {

  pcaRes = pca.calc(morphoMockup)

  expect_is(pcaRes, "pcadata")

  expect_error(plot(pcaRes, axes = c(3,33)), "specified axes are out of bounds. Object has only 2 axes." )

  #expect_error(plot(pcaRes, axes = c(1,1,2)), "you have to specifi 2 axes (e.g., axes = c(1,2))" )
  expect_error(plot(pcaRes, axes = c(1,1,2)) )

})
