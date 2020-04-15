context("samples")


test_that("correct info about class structure", {
  data = read.morphodata("../testFiles/sample.txt")
  output = capture.output(samples(data))

  expect_equal(output[1], "  [1] \"BABL1146\" \"BABL1147\" \"BABL1148\" \"BABL1149\" \"BABL1150\" \"BABL1151\"")

})




