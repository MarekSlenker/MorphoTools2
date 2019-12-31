morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8"),
                    "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4"),
                    "Taxon" = c("TaxA", "TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB", "TaxB"),
                    "data" = data.frame(
                      "Ch1" = 1:8,
                      "Ch2" = 11:18))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)


context("popul.otu")

test_that("correctness of calculation",  {


  mockup.OTU = popul.otu(morphoMockup)

  expect_is(mockup.OTU, "morphodata")

  expect_equal(paste(mockup.OTU$ID, collapse = " "), "Pop1 Pop2 Pop3 Pop4")

  expect_equal(paste(mockup.OTU$Population, collapse = " "), "Pop1 Pop2 Pop3 Pop4")

  expect_equal(paste(mockup.OTU$Taxon, collapse = " "), "TaxA TaxA TaxB TaxB")

  expect_equal(paste(mockup.OTU$data, collapse = " "), "c(1.5, 3.5, 5.5, 7.5) c(11.5, 13.5, 15.5, 17.5)")
})
