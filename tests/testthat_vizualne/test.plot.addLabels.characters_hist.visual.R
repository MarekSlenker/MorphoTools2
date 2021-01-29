context("plotAddLabels.characters_hist")

options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)

cdaRes = cda.calc(centaurea, passiveSamples = c("hybr", "ph"))


test_that("cda visual hist",  {

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(cdaRes, labels = F)
  plotAddLabels.characters(cdaRes, labels = c("MW", "IW", "SFT", "SF", "LW"), pos = 4, cex = 1, axes = 1)
  plotAddLabels.characters(cdaRes, labels = c("LLW", "ILW", "LBA"), pos = 2, cex = 1)
  plotAddLabels.characters(cdaRes, labels = c("ML", "IV", "MLW"), pos = 1, cex = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels.characters.hist1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(cdaRes, labels = F)
  plotAddLabels.characters(cdaRes, axes = 1, cex = 1,  col = "darkgreen")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLabels.characters.hist2.png"), threshold = 1)  )

})





