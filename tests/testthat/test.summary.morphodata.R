context("summary.morphodata")


test_that("input of different class", {
  data = read.delim("../testFiles/sample.txt")
  expect_error(summary.morphodata(data), "object is not of class 'morphodata'")
})

