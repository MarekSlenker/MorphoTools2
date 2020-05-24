context("plot.points.cdadata_hist")

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

test_that("visual 2 skupiny",  {

  cdaRes = cda.calc(morphoMockup)

  expect_is(cdaRes, "cdadata")

  expect_warning(plot.points(cdaRes, axes = c(1,3)), "The object has only one axis, which will be plotted")

  expect_warning(plot.points(cdaRes, axes = 4), "The object has only one axis, which will be plotted")

  expect_warning(plot.points(cdaRes, labels = T), "Labels = TRUE is not supported for histograms.")

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes, axes = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.hist.default.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes, col = c("red", "green"), legend = T )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.hist.col.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes,  pt.bg =  c("green", "red"), legend = T )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.hist.ptbg.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes,  pt.bg =  c("green", "red"), breaks = 0.2 )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.hist.breaks.png"), threshold = 1)  )

})




test_that("visual 2+ skupiny pasivne",  {

  cdaRes = cda.calc(centaurea, passiveSamples = c("hybr", "ph"))

  expect_warning(plot.points(cdaRes, axes = c(1,3)), "The object has only one axis, which will be plotted")

  expect_warning(plot.points(cdaRes, axes = 4), "The object has only one axis, which will be plotted")

  expect_warning(plot.points(cdaRes, labels = T), "Labels = TRUE is not supported for histograms.")

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes,  pt.bg =  c("green", "red", "yellow", "navy"), breaks = 0.2 ,axes = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.hist.centaturea.png"), threshold = 1)  )




  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.points(cdaRes,  col =  c("green", "red", "yellow", "navy"), legend = T, pch = c(1,3,4,6,7,8), breaks = (0.2))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.cda.hist.centaturea2.png"), threshold = 1)  )
  })


