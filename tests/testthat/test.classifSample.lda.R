context("classifSample.lda")

options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

trainingSet = deletePopulation(centaurea, populationName = "SOK")
SOK = keepPopulation(centaurea, populationName = "SOK")

options(warn=0)


test_that("correct input", {


})




test_that("correctness of calculation",  {
  options(warn=-1)
  classif.lda.SOK = classifSample.lda(SOK, trainingSet)
  options(warn=0)

  expect_is( classif.lda.SOK, "classifdata")
  expect_is( classif.lda.SOK$classif, "data.frame")
  expect_is( classif.lda.SOK$prob, "data.frame")
  expect_is( classif.lda.SOK$correct, "NULL")

  expect_equal(paste(classif.lda.SOK$prob, collapse = ","), "c(0.0019, 0.0656, 0.0631, 0.0527, 0.1497, 0.0463, 0.0116, 0.1127, 0.7483, 0.0457, 0.0807, 0.0815, 0.4452, 0.2073, 0.0019, 0.0132, 0.0194, 0.164, 0.0155, 0.1867),c(0.0072, 3e-04, 0, 0, 0, 0, 6e-04, 0, 2e-04, 0.0024, 1e-04, 0, 0, 0.0016, 1e-04, 0.0101, 0, 0, 0, 0),c(0.9909, 0.9339, 0.9369, 0.9467, 0.8503, 0.9537, 0.9878, 0.8873, 0.1827, 0.952, 0.9192, 0.9185, 0.5548, 0.7908, 0.9979, 0.9768, 0.9806, 0.836, 0.9835, 0.7219),c(0, 1e-04, 0, 6e-04, 0, 0, 0, 0, 0.0688, 0, 0, 0, 0, 3e-04, 0, 0, 0, 0, 0.001, 0.0913)")
  expect_equal(paste(classif.lda.SOK$correct, collapse = ","), "")
  expect_equal(paste(classif.lda.SOK$classif, collapse = ","), "c(\"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"hybr\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\", \"ps\")")
  expect_equal(paste(classif.lda.SOK$ID, collapse = ","), "SOK388,SOK389,SOK390,SOK391,SOK392,SOK393,SOK394,SOK395,SOK396,SOK397,SOK398,SOK399,SOK402,SOK403,SOK406,SOK409,SOK414,SOK415,SOK416,SOK417")
})

