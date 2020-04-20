context("plot.characters.cdadata_scatter")



# locally suppress warnings
options(warn=-1)
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)

cdaRes = cda.calc(centaurea)

test_that("ploting with error parameters",  {

  expect_error(plot.characters(cdaRes, axes = c(3,5)), "specfiied axes are out of bounds. Object has only 3 axes." )

  expect_error(plot.characters(cdaRes, axes = c(1,1,2))) # "you have to specifi 2 axes (e.g., axes = c(1,2))"
})

test_that("visual hist",  {

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.characters(cdaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.characters_cda_scatter_1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plot.characters(cdaRes, axes = c(2,3), xlab = "eee", main = "mmmmm", xlim = c(-2,0), ylim = c(-0.3,0),
                  col = "skyblue")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plot.characters_cda_scatter_2.png"), threshold = 1)  )
})
