context("export.res")

data = read.morphodata("../testFiles/sample_mockup.txt")


test_that("class morphodata: read and export same data",  {
  export.res(data, file = "../testFiles/sample_mockup.exported.txt")

  exportedData = read.morphodata("../testFiles/sample_mockup.exported.txt")

  expect_identical(data, exportedData)
})
