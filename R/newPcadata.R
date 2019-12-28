


newPcadata <- function() {

  newPcadata <- list(sdev = numeric(), center = numeric(), scale = numeric(), scores = numeric(),
                     eigenVectors = numeric(), eigenValues = numeric(), axesVariance = numeric(),
                     cumulativeAxesVariance = numeric())

  attr(newPcadata, "class") <- "pcadata"
  return(newPcadata)
}
