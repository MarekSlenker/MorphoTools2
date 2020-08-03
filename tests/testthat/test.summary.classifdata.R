context("summary.classifdata")


test_that("correct info about class structure", {
  data = read.morphodata("../testFiles/samplePlnaMatica.txt")

  options(warn=-1)
  c.lda = classif.lda(data)
  options(warn=0)

  output = capture.output(summary(c.lda))

  expect_equal(output[1], "object of class 'classifdata'; storing results of Classificatory Discriminant Analysis")
})




