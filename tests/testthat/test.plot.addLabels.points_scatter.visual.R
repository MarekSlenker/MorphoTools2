context("plotAddLabels.points_scatter")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxB", "TaxB", "TaxC", "TaxC", "TaxC", "TaxC"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))

morphoMockup = .morphodataFromDataFrame(morphoDataFrame)

bin = read.morphodata("../testFiles/bin.txt")

test_that("cda visual scatter",  {

  cdaRes = cda.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(cdaRes)
  plotAddLabels.points(cdaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_default.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(cdaRes)
  plotAddLabels.points(cdaRes, labels = c("id5", "id8"),pos = 2, cex = 2,  offset = 2, col = "red")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_include.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(cdaRes)
  plotAddLabels.points(cdaRes, include = F, labels = c("id5", "id8"),pos = 2, cex = 2,  offset = 2, col = c("red", "green"))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_exclude.png"), threshold = 1)  )
})

test_that("pca visual",  {

  pcaRes = pca.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes)
  plotAddLabels.points(pcaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_pca_default.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes)
  plotAddLabels.points(pcaRes, labels = c("id5", "id8"))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_pca_include.png"), threshold = 1)  )

})

test_that("pcoa visual",  {

  pcoaRes = pcoa.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcoaRes)
  plotAddLabels.points(pcoaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_pcoa_1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcoaRes)
  plotAddLabels.points(pcoaRes, labels = c("id5", "id8"))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_pcoa_2.png"), threshold = 1)  )


  pcoaRes = pcoa.calc(bin, distMethod = "jaccard")
  # pcoaRes$objects$scores = pcoaRes$objects$scores*-1

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcoaRes)
  plotAddLabels.points(pcoaRes, pos = 2, offset = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_pcoa_3.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcoaRes)
  plotAddLabels.points(pcoaRes, labels = c("1", "8"), pos = 2, offset = 1, cex = 3)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels_pcoa_4.png"), threshold = 1)  )


})




