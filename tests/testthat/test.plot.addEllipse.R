context("plot.addEllipses")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "data" = data.frame(
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18)))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)


options(warn=-1)
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)

test_that("plot.addEllipses error input",  {

  cdaRes = cda.calc(centaurea)

  plot.points(cdaRes, pch =c(18,16), col = c("red", "green", "blue", "black", "yellow"), cex = 0.5)

  expect_error(plot.addEllipses(cdaRes, axes = 1), "you have to specifi 2 axes (e.g., axes = c(1,2))", fixed = TRUE)
  expect_error(plot.addEllipses(cdaRes, axes = c(1,5)), "specified axes are out of bounds. Object has only 3 axes.")
  expect_error(plot.addEllipses(cdaRes, probability = 1), "Probability = 1 caused infinite size of ellipses.")

  cdaRes = cda.calc(morphoMockup)
  expect_error(plot.addEllipses(cdaRes), "The method plot.addEllipses() is not applicable to histogram.", fixed = TRUE)

  pcaRes = pca.calc(centaurea)
  expect_error(plot.addEllipses(pcaRes, axes = 1), "you have to specifi 2 axes (e.g., axes = c(1,2))", fixed = TRUE)
  expect_error(plot.addEllipses(pcaRes, axes = c(1,45)), "specified axes are out of bounds. Object has only 25 axes.")
})


test_that("plot.addEllipses visual",  {

  cdaRes = cda.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes, pch =c(18,16), col = c("red", "green", "blue", "black", "yellow"), cex = 0.5, legend = T)
  plot.addEllipses(cdaRes, col = c("red", "green", "blue", "black", "yellow"))
  plot.addEllipses(cdaRes, col = c("red", "green", "blue", "black", "yellow"), probability = 0.70, lty = 2)
  plot.addEllipses(cdaRes, col = c("red", "green", "blue", "black", "yellow"), probability = 0.999, type = "p",lwd = 3)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addEllipses1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes, pch =c(18,16), cex = 0.5, col = c("red", "green"))
  plot.addEllipses(cdaRes, col = c("red", "green"), lwd = 3)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.addEllipses2.png"), threshold = 1)  )
})




