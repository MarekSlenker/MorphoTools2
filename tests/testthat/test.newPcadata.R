context("newPcadata")

test_that("check class of new object",  {


  newObject = newPcadata()

  expect_is(newObject, "pcadata")

  expect_output(str(newObject), "List of 8")
})
