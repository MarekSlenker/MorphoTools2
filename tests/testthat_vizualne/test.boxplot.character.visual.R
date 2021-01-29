context("boxplotCharacter")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                             "data" = data.frame(
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18)))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)

test_that("visual",  {

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  boxplotCharacter(morphoMockup, "data.Ch1", 0.95, 0.05)
  dev.off()

  expect_true(  visualTest::isSimilar(tmp,  visualTest::getFingerprint("../testFiles/figs/boxplot.png"), threshold = 1)  )
})




