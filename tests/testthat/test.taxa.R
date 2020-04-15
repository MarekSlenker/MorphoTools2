context("taxa")


test_that("correct info about class structure", {
  data = read.morphodata("../testFiles/sample.txt")
  output = capture.output(taxa(data))

  expect_equal(output[1], "[1] \"hybr\" \"ph\"   \"ps\"   \"st\"  ")

})




