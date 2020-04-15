context("populations")


test_that("correct info about class structure", {
  data = read.morphodata("../testFiles/sample.txt")
  output = capture.output(populations(data))

  expect_equal(output[1], " [1] \"BABL\" \"BABU\" \"BOL\"  \"BRT\"  \"BUK\"  \"CERM\" \"CERV\" \"CZLE\" \"DEB\"  \"DOM\" ")

})




