context("plotCharacters.cdadata_scatter")



# locally suppress warnings
options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
options(warn=0)

cdaRes = cda.calc(centaurea)

test_that("visual hist",  {

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(cdaRes)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotCharacters_cda_scatter_1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(cdaRes, axes = c(2,3), xlab = "eee", main = "mmmmm", xlim = c(-2,0), ylim = c(-0.3,0),
                  col = "skyblue")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotCharacters_cda_scatter_2.png"), threshold = 1)  )
})
