context("plotCharacters.cdadata_hist")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)

# locally suppress warnings
options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
centaurea = deleteTaxon(centaurea, taxonName = c("hybr", "ph"))
options(warn=0)

cdaRes = cda.calc(centaurea)


test_that("visual hist",  {

  expect_warning(plotCharacters(cdaRes, axes = c(1,3)), "The object has only one axis, which will be plotted")

  expect_warning(plotCharacters(cdaRes, axes = 4), "The object has only one axis, which will be plotted")

    tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(cdaRes, axes = 1)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotCharacters_hist_1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotCharacters(cdaRes, axes = c(1), xlab = "eee", main = "mmmmm", xlim = c(-2,0), ylim = c(-0.3,0),
                  col = "green")
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotCharacters_hist_2.png"), threshold = 1)  )
})
