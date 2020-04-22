context("plot.addLabels.characters_scatter")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxB", "TaxB", "TaxC", "TaxC", "TaxC", "TaxC"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))
morphoMockup = morphodataFromDataFrame(morphoDataFrame)

options(warn=-1)
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)


test_that("pca wrong input",  {
  pcaRes = pca.calc(centaurea)

  plot.characters(pcaRes, labels = F)

  expect_error(plot.addLabels.characters(pcaRes, axes = 2), "you have to specifi 2 axes (e.g., axes = c(1,2))", fixed = TRUE)
  expect_error(plot.addLabels.characters(pcaRes, axes = c(2,26)), "specified axes are out of bounds. Object has only 25 axes.")

  expect_error(plot.addLabels.characters(pcaRes, labels = "eeee", pos = 4, cex = 1), "label eeee does not exist")

  expect_error(plot.addLabels.characters(pcaRes, include = F), "No labels to plot. You specified to exclude (include = FALSE) all labels", fixed = TRUE)

})


test_that("pca visual",  {

  pcaRes = pca.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.characters(pcaRes, labels = F)
  plot.addLabels.characters(pcaRes, labels = c("MW", "IW", "SFT", "SF", "LW"), pos = 2, cex = 1)
  plot.addLabels.characters(pcaRes, labels = c("LLW", "ILW", "LBA"), pos = 4, cex = 1)
  plot.addLabels.characters(pcaRes, labels = c("ML", "IV", "MLW"), pos = 1, cex = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels.characters.pca1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.characters(pcaRes, labels = F, axes = c(1,25))
  plot.addLabels.characters(pcaRes, axes = c(1,25), cex = 1,  col = "darkgreen")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels.characters.pca2.png"), threshold = 1)  )

})

test_that("cda visual scatter",  {

  cdaRes = cda.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.characters(cdaRes, labels = F)
  plot.addLabels.characters(cdaRes, labels = c("MW", "IW", "SFT", "SF", "LW"), pos = 2, cex = 1)
  plot.addLabels.characters(cdaRes, labels = c("LLW", "ILW", "LBA"), pos = 4, cex = 1)
  plot.addLabels.characters(cdaRes, labels = c("ML", "IV", "MLW"), pos = 1, cex = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels.characters.cda1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.characters(cdaRes, labels = F, axes = c(1,3))
  plot.addLabels.characters(cdaRes, axes = c(1,3), cex = 1,  col = "darkgreen")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addLabels.characters.cda2.png"), threshold = 1)  )

})




