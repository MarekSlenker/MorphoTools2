context("newPcadata")

test_that("check class of new object",  {


  newObject = newPcadata()

  expect_is(newObject, "pcadata")

  expect_named(newObject, c('objects', 'eigenVectors', 'eigenValues', 'eigenvaluesAsPercent', 'cumulativePercentageOfEigenvalues', 'groupMeans', 'rank', 'sdev', 'center', 'scale'))

  expect_named(newObject$objects, c('ID', 'Population', 'Taxon', 'scores'))


  expect_output(str(newObject), "List of 10")
})
