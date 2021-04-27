context("exportRes")


test_that("class morphodata: read and export same data",  {

  data = read.morphodata("../testFiles/sample_mockup.txt")

  exportRes(data, file = "../testFiles/sample_mockup.exported.txt")

  exportedData = read.morphodata("../testFiles/sample_mockup.exported.txt")

  expect_identical(data, exportedData)
})


test_that("class data.frame: read and export same data",  {

  morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                               "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                               "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                               "data" = data.frame(
                                 "Ch1" = 1:8,
                                 "Ch2" = 11:18))

  exportRes(morphoDataFrame, file = "../testFiles/morphoDataFrame.exported.txt")

  morphoDataFrame.imported = read.morphodata("../testFiles/morphoDataFrame.exported.txt")

  morphoDataFrame.morphodata = .morphodataFromDataFrame(morphoDataFrame)

  expect_identical(morphoDataFrame.imported, morphoDataFrame.morphodata)
})

test_that("class data.frame: compare as tables",  {

  morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                               "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                               "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                               "data" = data.frame(
                                 "Ch1" = 1:8,
                                 "Ch2" = 11:18))

  exportRes(morphoDataFrame, file = "../testFiles/morphoDataFrame.exported.txt")

  DFImported = read.delim("../testFiles/morphoDataFrame.exported.txt")

  expect_identical(morphoDataFrame, DFImported)
})
