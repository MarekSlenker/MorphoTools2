context("read.morphodata: unvalid input")

test_that("input file is too short", {
  expect_error(read.morphodata("../testFiles/shortFile.txt"), "incorrect data format")
})

