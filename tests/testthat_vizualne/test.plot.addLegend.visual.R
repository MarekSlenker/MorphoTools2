context("plotAddLegend")

morphoDataFrame = data.frame("ID" = c("id1","id2","id3","id4","id5","id6","id7","id8","id9"),
                             "Population" = c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3", "Pop4", "Pop4", "Pop5"),
                             "Taxon" = c("TaxA", "TaxA", "TaxB", "TaxB", "TaxC", "TaxC", "TaxD", "TaxD", "TaxE"),
                               "Ch1" = c(1,3,4,6,1,7,12,8,11),
                               "Ch2" = c(11, 12,42,12,32,11,22,18,12))

morphoMockup = morphodataFromDataFrame(morphoDataFrame)

test_that("addlegend visual",  {

  cdaRes = cda.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(cdaRes, pch =c(18,16), col = c("red", "green", "blue", "black", "yellow"), cex = 2)
  plotAddLabels.points(cdaRes)
  plotAddLegend(cdaRes, pch =c(18,16), x = "bottomleft", pt.cex = 1.5,
                 col = c("red", "green", "blue", "black", "yellow", "navy"))
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLegend1.png"), threshold = 1)  )

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(cdaRes, pch =c(18,16), col = c("red", "green", "blue", "black", "yellow"), cex = 2)
  plotAddLabels.points(cdaRes, offset = 0.5, pos = 2, labels = "id7", include = F)
  plotAddLabels.points(cdaRes, offset = 0.5, pos = 4, labels = "id7", include = T)
  plotAddLegend(cdaRes, pch =c(24,16), x = "bottomleft", pt.cex = 1.5,
                 col = c("red", "green", "blue", "black", "navy"),
            pt.bg = "salmon", x.intersp = 1.4, y.intersp = 1.5, box.type = "n"     )
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLegend_boxN.png"), threshold = 1)  )



  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(cdaRes, pch =c(18,16), col = c("red", "green", "blue", "black", "yellow"), cex = 2)
  plotAddLabels.points(cdaRes, offset = 0.5, pos = 2, labels = "id7", include = F)
  plotAddLabels.points(cdaRes, offset = 0.5, pos = 4, labels = "id7", include = T)
  plotAddLegend(cdaRes, pch =c(24,16), x = "bottomleft", pt.cex = 1.5,
                 col = c("red", "green", "blue", "black", "navy"),
                 pt.bg = "salmon", box.bg = "grey", box.lwd = 2, box.lty = 2, box.col = "#FF0000", cex = 1.5, horiz = T)
  plotAddLegend(cdaRes, pch =c(24,16), x = "bottomleft", pt.cex = 1.5,
                 col = c("red", "green", "blue", "black", "navy"),
                 pt.bg = "salmon", box.bg = "grey", box.lwd = 2, box.lty = 2, box.col = "#FF0000", cex = 1.5, horiz = F,
                 ncol = 3)

  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLegend_horiz.png"), threshold = 1)  )


  pcaRes = pca.calc(morphoMockup)

  tmp  = tempfile(fileext = ".png")
  png(filename = tmp, width = 400, height = 400)
  plotPoints(pcaRes, pch =c(18,16), col = c("red", "green", "blue", "black", "yellow"), cex = 2)
  plotAddLabels.points(pcaRes, offset = 0.5, pos = 2, labels = c("id7", "id3", "id5", "id6"), include = F)
  plotAddLabels.points(pcaRes, offset = 0.5, pos = 4, labels = c("id7", "id3", "id5", "id6"), include = T)
  plotAddLegend(pcaRes, pch =c(24,16), x = -1.5, y = 1, pt.cex = 1.5,
                 col = c("red", "green", "blue", "black", "navy"),
                 pt.bg = "salmon", box.bg = "grey", box.lwd = 2, box.lty = 2, box.col = "#FF0000", ncol = 3)
  dev.off()
  expect_true(visualTest::isSimilar(tmp,visualTest::getFingerprint("../testFiles/figs/plotAddLegend_11.png"), threshold = 1)  )

})




