context("pca.calc")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))

morphoDataFrameConstant = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "Ch1" = c(1,3,4,6,1,7,12,8),
                             "Ch2" = c(11, 11,11,11,11,11,11,11))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)
constantMockup = morphodataFromDataFrame(morphoDataFrameConstant)

test_that("correctness of calculation",  {
  pca_prconp = prcomp(morphoDataFrame[,4:5], center=T, scale.=T)

  pcaRes = pca.calc(morphoMockup)

  expect_is(pcaRes, "pcadata")

  expect_equal(pcaRes$sdev, pca_prconp$sdev)


  expect_equal(pcaRes$center, pca_prconp$center)
  expect_equal(pcaRes$scale, pca_prconp$scale)
  expect_equal(as.data.frame(pcaRes$objects$scores, row.names = 1), as.data.frame(pca_prconp$x, row.names = 1))
  expect_equal(pcaRes$eigenVectors, pca_prconp$rotation)
  expect_equal(pcaRes$eigenValues, sapply(pca_prconp$sdev,function(x) x^2))
  expect_true(is.numeric(pcaRes$axesVariance))
  expect_true(is.numeric(pcaRes$cumulativeAxesVariance))
})

test_that("constant values",  {

  expect_error(pca.calc(constantMockup), "Characters Ch2 are constant.")


})
