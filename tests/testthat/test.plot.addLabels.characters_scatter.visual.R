context("plotAddLabels.characters_scatter")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxB", "TaxB", "TaxC", "TaxC", "TaxC", "TaxC"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))
morphoMockup = .morphodataFromDataFrame(morphoDataFrame)

options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)



test_that("pca visual",  {

  pcaRes = pca.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(pcaRes, labels = F)
  plotAddLabels.characters(pcaRes, labels = c("MW", "IW", "SFT", "SF", "LW"), pos = 2, cex = 1)
  plotAddLabels.characters(pcaRes, labels = c("LLW", "ILW", "LBA"), pos = 4, cex = 1)
  plotAddLabels.characters(pcaRes, labels = c("ML", "IV", "MLW"), pos = 1, cex = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels.characters.pca1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(pcaRes, labels = F, axes = c(1,25))
  plotAddLabels.characters(pcaRes, axes = c(1,25), cex = 1,  col = "darkgreen")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels.characters.pca2.png"), threshold = 1)  )

})

test_that("cda visual scatter",  {

  cdaRes = cda.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(cdaRes, labels = F)
  plotAddLabels.characters(cdaRes, labels = c("MW", "IW", "SFT", "SF", "LW"), pos = 2, cex = 1)
  plotAddLabels.characters(cdaRes, labels = c("LLW", "ILW", "LBA"), pos = 4, cex = 1)
  plotAddLabels.characters(cdaRes, labels = c("ML", "IV", "MLW"), pos = 1, cex = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels.characters.cda1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(cdaRes, labels = F, axes = c(1,3))
  plotAddLabels.characters(cdaRes, axes = c(1,3), cex = 1,  col = "darkgreen")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels.characters.cda2.png"), threshold = 1)  )

})




