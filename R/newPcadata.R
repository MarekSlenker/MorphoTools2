


newPcadata <- function() {

  newPcadata <- list(sdev = numeric(),
                     center = numeric(),
                     scale = numeric(),
                     objects = list(
                          ID = numeric(),
                          Population = numeric(),
                          Taxon = numeric(),
                          scores = numeric()),
                     eigenVectors = numeric(),
                     eigenValues = numeric(),
                     axesVariance = numeric(),
                     cumulativeAxesVariance = numeric())

  attr(newPcadata, "class") <- "pcadata"
  return(newPcadata)
}
