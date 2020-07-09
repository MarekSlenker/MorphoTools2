context("plot.3Dpoints.cdadata")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "Ch1" = c(1,3,4,6,1,7,12,8),
                             "Ch2" = c(11, 12,42,12,32,11,22,18))
morphoMockup = morphodataFromDataFrame(morphoDataFrame)

# locally suppress warnings
options(warn=-1)
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)

pops=popul.otu(centaurea)


test_that("ploting with error parameters",  {

  cdaRes = cda.calc(morphoMockup)

  expect_error(plot.3Dpoints(cdaRes), "3D plot requires at least 3 axes. Object has 1 axes." )
})

test_that("visual",  {

  cdaRes = cda.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.3Dpoints(cdaRes, col = c("red", "green"), phi = 10, theta = 22, pch = c(1,11), bty = "b2", legend = T)
  dev.off()
  testthat::expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot3D.points.cda1.png"), threshold = 1)  )


  popsRes= cda.calc(pops)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.3Dpoints(popsRes, col = c("red", "green"), phi = 20, theta = 2, labels = T)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot3D.points.cda2.png"), threshold = 1)  )

})
