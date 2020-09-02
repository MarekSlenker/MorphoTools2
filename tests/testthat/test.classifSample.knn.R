context("classifSample.knn")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                             "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                               "Ch1" = c(1,3,4,6,1,7,12,8),
                               "Ch2" = c(11, 12,42,12,32,11,22,18))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)


test_that("correct input - NA values", {
  trainingDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                                 "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                                 "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                                 "data" = data.frame(
                                   "Ch1" = c(1,3,NA,6,1,7,12,8),
                                   "Ch2" = c(11, 12,42,12,32,11,22,18)))

  sampDataFrame = data.frame("ID" = c("id1X","id2X"),
                             "Population" = c("PopX", "PopX"),
                             "Taxon" = c("TaxX", "TaxX"),
                             "data" = data.frame(
                               "Ch1" = c(11,13),
                               "Ch2" = c(31, 32)))

  trainingMockup = morphodataFromDataFrame(trainingDataFrame)
  sampMockup = morphodataFromDataFrame(sampDataFrame)

  expect_error(classifSample.knn(sampMockup, trainingMockup, k = 6), "NA values in 'trainingData'.")

  ##############x

  trainingDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                                 "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                                 "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                                 "data" = data.frame(
                                   "Ch1" = c(1,3,3,6,1,7,12,8),
                                   "Ch2" = c(11, 12,42,12,32,11,22,18)))

  sampDataFrame = data.frame("ID" = c("id1X","id2X"),
                             "Population" = c("PopX", "PopX"),
                             "Taxon" = c("TaxX", "TaxX"),
                             "data" = data.frame(
                               "Ch1" = c(NA,13),
                               "Ch2" = c(31, 32)))

  trainingMockup = morphodataFromDataFrame(trainingDataFrame)
  sampMockup = morphodataFromDataFrame(sampDataFrame)

  expect_error(classifSample.knn(sampMockup, trainingMockup, k = 4), "NA values in 'sampleData'.")

})



test_that("correct input - different characters", {
  trainingDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                                 "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                                 "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                                 "data" = data.frame(
                                   "Ch1" = c(1,3,3,6,1,7,12,8),
                                   "Ch2" = c(11, 12,42,12,32,11,22,18)))

  sampDataFrame = data.frame("ID" = c("id1X","id2X"),
                             "Population" = c("PopX", "PopX"),
                             "Taxon" = c("TaxX", "TaxX"),
                             "data" = data.frame(
                               "ChX" = c(11,13),
                               "Ch2" = c(31, 32)))

  trainingMockup = morphodataFromDataFrame(trainingDataFrame)
  sampMockup = morphodataFromDataFrame(sampDataFrame)

  expect_error(classifSample.knn(sampMockup, trainingMockup, k = 5), "Characters of 'sampleData' and 'trainingData' are not the same.")

})

test_that("correctness of calculation",  {
  options(warn=-1)
  c = classif.knn(morphoMockup, k=2)
  options(warn=0)

  expect_is( c, "classifdata")
  expect_is( c$classif, "data.frame")
  expect_is( c$prob, "data.frame")
  expect_is( c$correct, "data.frame")

  expect_equal( attr(c, "method"), "knn")

  expect_equal(paste(c$prob, collapse = ","), "c(1, 1, 1, 0.5, 0.5, 1, 0.5, 0.5)")
  #expect_equal(paste(c$correct, collapse = ","), "c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)")
  #expect_equal(paste(c$classif, collapse = ","), "c(\"TaxA\", \"TaxA\", \"TaxB\", \"TaxB\", \"TaxA\", \"TaxB\", \"TaxB\", \"TaxA\")")
  expect_equal(paste(c$ID, collapse = ","), "id1,id2,id3,id4,id5,id6,id7,id8")
})


test_that("correctness of calculation  -to iste 2 metodami",  {

  data = read.morphodata("../testFiles/samplePlnaMatica.txt")


  bezRTE = deletePopulation(data, populationName = "RTE")
  RTE = keepPopulation(data, populationName = "RTE")

  options(warn=-1)
  RTE.classif = classifSample.knn(RTE, bezRTE, k = 2)
  class_vsetko = classif.knn(data, k = 2, crossval = "pop")
  options(warn=0)

  expect_equal(class_vsetko$ID[473:492], RTE.classif$ID)
  expect_equal(class_vsetko$Population[473:492], RTE.classif$Population)
  #expect_equal(paste(class_vsetko$classif[473:492,]), paste(RTE.classif$classif[1:20,]))
  expect_equal(class_vsetko$prob[473:492,], RTE.classif$prob[,])

})
