context("plot.points.cdadata_scatter")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxB", "TaxB", "TaxC", "TaxC", "TaxC", "TaxC"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)

# locally suppress warnings
options(warn=-1)
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)


test_that("ploting with error parameters",  {

  cdaRes = cda.calc(morphoMockup)

  expect_is(cdaRes, "cdadata")

  expect_error(plot.points(cdaRes, axes = c(3,33)), "specified axes are out of bounds. Object has only 2 axes." )

  expect_error(plot.points(cdaRes, axes = c(1,1,2))) # "you have to specifi 2 axes (e.g., axes = c(1,2))"
})

test_that("visual",  {

  cdaRes = cda.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.scatter.default.png"), threshold = 1)  )


  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes, pch = c(15,17), axes = c(2,1),  col = c("green", "red", "navy"), cex = 1.4, legend = T, labels = T)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.scatter.colCexLabLeg.png"), threshold = 1)  )

  cdaRes = cda.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes, pch = c(19,18), legend = T, col = c("green", "red", "orange", "navy") , cex = 0.7, labels = T )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.scatter.centaurea1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes, pch = c(22), legend = T, pt.bg = c("green", "red"), col = c("green", "red", "orange", "navy"), cex = 1.4 )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.scatter.centaurea2.png"), threshold = 1)  )
})
