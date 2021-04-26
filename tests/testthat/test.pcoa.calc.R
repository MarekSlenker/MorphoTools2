context("pcoa.calc")

imp = read.morphodata("../testFiles/Impatiens_Pektinata.txt")

test_that("correctness of calculation",  {
  pcoa_cmdscale = stats::cmdscale(stats::dist( scale(imp$data, center = TRUE, scale = TRUE)), k = 3, eig = TRUE, x.ret = TRUE)


  pcoaRes = pcoa.calc(imp, distMethod = "euclidean")

  expect_is(pcoaRes, "pcoadata")


  names(pcoa_cmdscale$eig) = names(pcoaRes$eigenValues)
  expect_equal( pcoaRes$eigenValues, pcoa_cmdscale$eig[1:pcoaRes$rank])

  expect_equal(pcoaRes$distMethod, "euclidean")

  expect_equal(paste(pcoaRes$objects$scores[1:5,1], collapse = ""), "-2.58440793355984-2.49362396964195-2.61614101238806-2.69556703915561-1.71592266465019")

  expect_equal(paste(pcoaRes$groupMeans[1:3,2], collapse = ""), "-2.287717094199942.66900327656659NA")


  pcoaRes = pcoa.calc(imp, distMethod = "jaccard")
  expect_equal(pcoaRes$distMethod, "jaccard")





})

