context("calcDistance")



morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "Ch1" = c(1:8),
                             "Ch2" = 11:18,
                             "Ch3" = c(22, 21, 23, 3, 44, 1, 8, 8 ),
                             "Ch4" = rep(1, 8))


morphoDataFrame_bin = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "Ch1" = c(1, 0, 0, 1, 0, 0, 1, 0),
                             "Ch2" = c(0, 1, 0, 1, 0, 1, 1, 1),
                             "Ch3" = c(0, 0, 1, 0, 0, 0, 0, 1),
                             "Ch4" = rep(1, 8))


morphoMockup = .morphodataFromDataFrame(morphoDataFrame)
morphoMockup.bin = .morphodataFromDataFrame(morphoDataFrame_bin)


test_that("correct info about class structure", {


  d1 = .calcDistance(morphoMockup, distMethod = "euclidean", scale = T, center = T)
  expect_equal(paste(d1[1:5], collapse = ","), "0.671593708886502,1.33580366938756,2.52592818565872,3.20974222499387,3.74418449597971")
  expect_equal(paste(d1[12:17], collapse = ","), "3.49648974814622,4.13694687514603,1.75553712379927,2.16462103071219,2.6816662804677,2.931669459488")

  d2 = .calcDistance(morphoMockup, distMethod = "jaccard", scale = T, center = T)
  expect_equal(paste(d2[1:5], collapse = ","), "0,0,0,0,0")

  d3 = .calcDistance(morphoMockup, distMethod = "simpleMatching", scale = T, center = T)
  expect_equal(paste(d3[15:25], collapse = ","), "0,0,0,0,0,0,0,0,0,0,0")

  expect_error( .calcDistance(morphoMockup, distMethod = "ee"), "distMethod ee is not supported.")

  # ostatne?

  d1.bin = .calcDistance(morphoMockup.bin, distMethod = "euclidean", scale = T, center = T)
  expect_equal(paste(d1.bin[1:5], collapse = ","), "3.15524255098646,3.3466401061363,2.23109340409087,2.23109340409087,3.15524255098646")

  d2.bin = .calcDistance(morphoMockup.bin, distMethod = "jaccard", scale = T, center = T)
  expect_equal(paste(d2.bin[1:5], collapse = ","), "0.816496580927726,0.816496580927726,0.577350269189626,0.707106781186548,0.816496580927726")

  d3.bin = .calcDistance(morphoMockup.bin, distMethod = "simpleMatching", scale = T, center = T)
  expect_equal(paste(d3.bin[15:25], collapse = ","), "0.5,0.707106781186548,0.866025403784439,0.5,0.707106781186548,0.5,0,0.707106781186548,0.5,0.707106781186548,0.707106781186548")

})



