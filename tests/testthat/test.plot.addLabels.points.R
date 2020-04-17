context("plot.addLabels.points")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxB", "TaxB", "TaxC", "TaxC", "TaxC", "TaxC"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)

test_that("cda visual 1",  {

  cdaRes = cda.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes)
  plot.addLabels.points(cdaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels_default.png"), threshold = 1)  )
})


test_that("cda visual 2",  {

  cdaRes = cda.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes)
  plot.addLabels.points(cdaRes, labels = c("id5", "id8"),pos = 2, cex = 2,  offset = 2, col = "red")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels_include.png"), threshold = 1)  )
})

test_that("cda visual 3",  {

  cdaRes = cda.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes)
  plot.addLabels.points(cdaRes, include = F, labels = c("id5", "id8"),pos = 2, cex = 2,  offset = 2, col = c("red", "green"))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels_exclude.png"), threshold = 1)  )
})

test_that("pca visual",  {

  pcaRes = pca.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(pcaRes)
  plot.addLabels.points(pcaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels_pca_default.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(pcaRes)
  plot.addLabels.points(pcaRes, labels = c("id5", "id8"))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels_pca_include.png"), threshold = 1)  )

})




