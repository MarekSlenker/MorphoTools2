context("plotAddSpiders")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "data" = data.frame(
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18)))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)


options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)


test_that("plotAddSpiders visual",  {

  cdaRes = cda.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(cdaRes, pch =c(18,16), col = c("red", "green", "blue", "black", "yellow"), cex = 0.5)
  plotAddSpiders(cdaRes, col = c("red", "green", "blue", "black", "yellow"), lty = 2, lwd = 2)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddSpiders1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(cdaRes, pch =c(18,16), cex = 0.5, col = c("red", "green"))
  plotAddSpiders(cdaRes, col = c("green", "red"))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddSpiders2.png"), threshold = 1)  )

  pcaRes = pca.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes, pch =c(18,16), cex = 0.5, col = c("red", "green", "blue", "black", "yellow"))
  plotAddSpiders(pcaRes, col=c(rgb(255,0,0,max=255,alpha=100),  rgb(0, 0, 255, max = 255, alpha = 100)), lwd = 3)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddSpiders3.png"), threshold = 1)  )

})




