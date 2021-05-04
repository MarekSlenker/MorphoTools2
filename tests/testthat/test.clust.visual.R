context("clust visual")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "data" = data.frame(
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18)))

morphoDataFrame_NA = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "data" = data.frame(
                               "Ch1" = c(1,3,NA,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18)))

morphoMockup = .morphodataFromDataFrame(morphoDataFrame)
morphoMockup_NA = .morphodataFromDataFrame(morphoDataFrame_NA)

gowerMockup = list(
  ID = as.factor(c("id1","id2","id3","id4","id5","id6")),
  Population = as.factor(c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3")),
  Taxon = as.factor(c("TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB")),
  data = data.frame(
    stemBranching = c(1, 1, 1, 0, 0, 0),   # binaryChs
    petalColour = c(1, 1, 2, 3, 3, 3),     # nominalChs; 1=white, 2=red, 3=blue
    leaves = c(1, 1, 1, 2, 2, 3),          # nominalChs; 1=simple, 2=palmately compound, 3=pinnately compound
    taste = c(2, 2, 2, 3, 1, 1),           # ordinal; 1=hot, 2=hotter, 3=hottest
    stemHeight = c(10, 11, 14, 22, 23, 21),         # quantitative
    leafLength = c(8, 7.1, 9.4, 1.2, 2.3, 2.1)  )   # quantitative
)
attr(gowerMockup, "class") <- "morphodata"


test_that("visual",  {

  clustRes = clust(morphoMockup, clustMethod = "UPGMA")

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot(clustRes)
  dev.off()

  expect_true(  visualTest::isSimilar(tmp,  visualTest::getFingerprint("../testFiles/figs/clust_default.png"), threshold = 1)  )


  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot(clustRes, cex=0.6, frame.plot=T, hang=-1, main="", sub="", xlab="", ylab="distance")
  dev.off()

  expect_true(  visualTest::isSimilar(tmp,  visualTest::getFingerprint("../testFiles/figs/clust_2.png"), threshold = 1)  )


  clustering.GOWER = clust(gowerMockup, distMethod = "gower", clustMethod = "UPGMA",
                           binaryChs = c("stemBranching"),
                           nominalChs = c("petalColour", "leaves"),
                           ordinalChs = c("taste"))
  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot(clustering.GOWER, cex=0.6, frame.plot=T, hang=-1, main="gower", sub="", xlab="", ylab="gower distance")
  dev.off()

  expect_true(  visualTest::isSimilar(tmp,  visualTest::getFingerprint("../testFiles/figs/clust_gower.png"), threshold = 1)  )


})


























