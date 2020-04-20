context("plot.characters.pcadata")

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


test_that("ploting with error parameters",  {

  pcaRes = pca.calc(centaurea)


  expect_error(plot.characters(centaurea, axes = c(3,5)), "no applicable method for 'plot.characters' applied to an object of class \"morphodata\"" )

  expect_error(plot.characters(pcaRes, axes = c(1,1,2))) # "you have to specifi 2 axes (e.g., axes = c(1,2))"

  expect_error(plot.characters(pcaRes, axes = c(3,55)), "specified axes are out of bounds. Object has only 25 axes." )

})

test_that("visual",  {

  pcaRes = pca.calc(centaurea)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.characters(pcaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.characters_default.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.characters(pcaRes, axes = c(2,4), xlab = "eee", main = "mmmmm", xlim = c(-1,1), ylim = c(-0.3,0),
                  col = "green")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.characters_2.png"), threshold = 1)  )
})
