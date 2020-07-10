context("plotPoints.pcadata")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)

# locally suppress warnings
options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)


test_that("ploting with error parameters",  {

  pcaRes = pca.calc(morphoMockup)

  expect_is(pcaRes, "pcadata")

  expect_error(plotPoints(pcaRes, axes = c(3,33)), "specified axes are out of bounds. Object has only 2 axes." )

  expect_error(plotPoints(pcaRes, axes = c(1,1,2))) # "you have to specifi 2 axes (e.g., axes = c(1,2))"
})

test_that("visual",  {

  pcaRes = pca.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.pca.default.png"), threshold = 1)  )


  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes, axes = c(2,1))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.pca.axes.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes, pch = c(2,8))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.pca.pch.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes, pch = c(19,18), col = ("green"), cex = 1.4)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.pca.col1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes, pch = c(19,18),labels = T, legend = T,ncol = 2, legend.pos = "center", col = c("green", "red") , cex = 1.4 )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.pca.col2.png"), threshold = 1)  )


  pcaRes = pca.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes, pch = c(19,18), legend = T, col = c("green", "red", "orange", "navy") , cex = 1.4 )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.pca.cent1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes, pch = c(22), legend = T, pt.bg = c("green", "red"), col = c("green", "red", "orange", "navy"), cex = 1.4 )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.pca.cent2.png"), threshold = 1)  )
})
