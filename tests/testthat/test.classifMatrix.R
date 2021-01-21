context("classif.matrix")

options(warn=-1)
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

trainingSet = deletePopulation(centaurea, populationName = "SOK")
SOK = keepPopulation(centaurea, populationName = "SOK")

options(warn=0)


test_that("correct input", {
  options(warn=-1)
  c = classif.lda(centaurea)
  options(warn=0)

  expect_error(classif.matrix(c, level = "ds"), "Invalid level of grouping. Consider using \"taxon\", \"pop\" or \"indiv\"")
})


test_that("classif.lda",  {
  options(warn=-1)
  c = classif.lda(centaurea)
  options(warn=0)

  m = classif.matrix(c, level = "taxon")
  expect_equal(colnames(m), c("Taxon", "N","as.hybr","as.ph","as.ps","as.st","correct","correct[%]"))
  expect_equal(rownames(m), c("1", "2", "3", "4", "5"))
  expect_equal(m[,1], c("hybr", "ph", "ps", "st", "Total"))

  m = classif.matrix(c, level = "pop")
  expect_equal(colnames(m), c("Population", "Taxon", "N","as.hybr","as.ph","as.ps","as.st","correct","correct[%]"))
  expect_equal(rownames(m)[1:5], c("1", "2", "3", "4", "5"))
  expect_equal(m[,1][1:5], c("BABL","BABU","BOL", "BRT", "BUK" ))
  expect_equal(m[,1][30:32], c("VIT",   "VOL",   "Total"))

  m = classif.matrix(c, level = "indiv")
  expect_equal(colnames(m), c("ID", "Population", "Taxon", "classification", "as.hybr", "as.ph",
                              "as.ps", "as.st", "correct"))
  expect_equal(rownames(m)[1:5], c("1", "2", "3", "4", "5"))
  expect_equal(m[,1][1:5], c("RTE1", "RTE2", "RTE3", "RTE4", "RTE5" ))
  expect_equal(m[,1][612], "KOT2295")

})

test_that("classif.knn",  {
  options(warn=-1)
  c = classif.knn(centaurea, k = 6)
  options(warn=0)

  m = classif.matrix(c, level = "taxon")
  expect_equal(colnames(m), c("Taxon", "N","as.hybr","as.ph","as.ps","as.st","correct","correct[%]"))
  expect_equal(rownames(m), c("1", "2", "3", "4", "5"))
  expect_equal(m[,1], c("hybr", "ph", "ps", "st", "Total"))

  m = classif.matrix(c, level = "pop")
  expect_equal(colnames(m), c("Population", "Taxon", "N","as.hybr","as.ph","as.ps","as.st","correct","correct[%]"))
  expect_equal(rownames(m)[1:5], c("1", "2", "3", "4", "5"))
  expect_equal(m[,1][1:5], c("BABL","BABU","BOL", "BRT", "BUK" ))
  expect_equal(m[,1][30:32], c("VIT",   "VOL",   "Total"))

  m = classif.matrix(c, level = "indiv")
  expect_equal(colnames(m), c("ID", "Population", "Taxon", "classification", "Proportion.of.the.votes.for.the.winning.class", "correct"))
  expect_equal(rownames(m)[1:5], c("1", "2", "3", "4", "5"))
  expect_equal(m[,1][1:5], c("RTE1", "RTE2", "RTE3", "RTE4", "RTE5" ))
  expect_equal(m[,1][612], "KOT2295")

})

test_that("classifSamp.lda",  {
  options(warn=-1)
  c = classifSample.lda(SOK, trainingSet)
  options(warn=0)

  m = classif.matrix(c, level = "taxon")
  expect_equal(colnames(m), c("Taxon", "N","as.hybr","as.ps"))
  expect_equal(rownames(m), c("1", "2"))
  expect_equal(m[,1], c("ps","Total"))

  m = classif.matrix(c, level = "pop")
  expect_equal(colnames(m), c("Population", "Taxon", "N","as.hybr","as.ps"))
  expect_equal(rownames(m), c("1", "2"))
  expect_equal(m[,1], c("SOK","Total" ))

  m = classif.matrix(c, level = "indiv")
  expect_equal(colnames(m), c("ID", "Population", "Taxon", "classification", "as.hybr", "as.ph",
                              "as.ps", "as.st"  ))
  expect_equal(rownames(m)[1:5], c("1", "2", "3", "4", "5"))
  expect_equal(m[,1][1:5], c("SOK388", "SOK389", "SOK390", "SOK391", "SOK392"))

})

test_that("classifSamp.knn",  {
  options(warn=-1)
  c = classifSample.knn(SOK, trainingSet, k = 4)
  options(warn=0)

  m = classif.matrix(c, level = "taxon")
  expect_equal(colnames(m), c("Taxon", "N","as.hybr","as.ph", "as.ps", "as.st"))
  expect_equal(rownames(m), c("1", "2"))
  expect_equal(m[,1], c("ps","Total"))

  m = classif.matrix(c, level = "pop")
  expect_equal(colnames(m), c("Population", "Taxon", "N","as.hybr", "as.ph", "as.ps", "as.st" ))
  expect_equal(rownames(m), c("1", "2"))
  expect_equal(m[,1], c("SOK","Total" ))

  m = classif.matrix(c, level = "indiv")
  expect_equal(colnames(m), c("ID", "Population", "Taxon", "classification", "Proportion.of.the.votes.for.the.winning.class"  ))
  expect_equal(rownames(m)[1:5], c("1", "2", "3", "4", "5"))
  expect_equal(m[,1][1:5], c("SOK388", "SOK389", "SOK390", "SOK391", "SOK392"))

})
