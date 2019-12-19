context("delete.charecter")

data = read.morphodata("../testFiles/sample.txt")

test_that("trying to remove unexisting character",  {
  expect_error(delete.charecter(data, "unexistingCh"), "charecter unexistingCh does not exist")
})


test_that("remove one character",  {
  subData = delete.charecter(data, "LL")

  expect_equal(dim(data$data)[1], dim(subData$data)[1])

  expect_equal(dim(data$data)[2] - 1, dim(subData$data)[2])

  expect_equal(paste(subData$data[1,], collapse = " "), "35.2 23.6 58.8 0.4 3.9 2.87 0 1 0 0 1.7 1.3 1.31 0 13.6 0.5 25.33 16 1 1 1 NA NA NA")

  expect_output(str(subData), "List of 4")
  expect_is(subData, "morphodata")
})

test_that("remove more characters",  {
  subData = delete.charecter(data, c("LL", "LLW", "MLW"))

  expect_equal(dim(data$data)[1], dim(subData$data)[1])

  expect_equal(dim(data$data)[2] - 3, dim(subData$data)[2])

  expect_equal(paste(subData$data[1,], collapse = " "), "35.2 23.6 58.8 0.4 3.9 0 1 0 0 1.7 1.3 1.31 0 13.6 0.5 16 1 1 1 NA NA NA")

  expect_output(str(subData), "List of 4")
  expect_is(subData, "morphodata")
})

