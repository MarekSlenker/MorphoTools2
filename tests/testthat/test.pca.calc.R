context("pca.calc")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "data" = data.frame(
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18)))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)

export.res(morphoMockup, file = "~/PCA.morphoMockup.txt")

test_that("correctness of calculation",  {

  pcaRes = pca.calc(morphoMockup)

  expect_is(pcaRes, "pcadata")

  expect_equal(paste(pcaRes$sdev, collapse = " "), "1.05763898021416 0.938828944766594")
  expect_equal(paste(pcaRes$center, collapse = " "), "5.25 20")
  expect_equal(paste(pcaRes$scale, collapse = " "), "3.77018377725619 11.5015526902116")
  expect_equal(paste(pcaRes$objects, collapse = " "), "1:8 c(1, 1, 2, 2, 3, 3, 4, 4) c(1, 1, 1, 1, 2, 2, 2, 2) c(-0.243784226178355, 0.0698412038544949, -1.58698382264512, 0.632498217116449, -1.53484838115588, 0.881529800345555, 1.14301978888916, 0.638727419773699, -1.35041064473052, -0.913826723747427, 1.11810297826016, -0.35116971048547, -0.0593464897529989, -0.225096618206605, 1.38893677078964, 0.39281043787322)")
  expect_equal(paste(pcaRes$eigenVectors, collapse = " "), "0.707106781186546 -0.707106781186549 0.707106781186549 0.707106781186546")
  expect_equal(paste(pcaRes$eigenValues, collapse = " "), "1.11860021246844 0.881399787531556")
  expect_equal(paste(pcaRes$axesVariance, collapse = " "), "0.5593 0.4407")
  expect_equal(paste(pcaRes$cumulativeAxesVariance, collapse = " "), "0.5593 1")
})
