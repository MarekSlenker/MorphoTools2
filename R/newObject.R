
# constructors

# internal
# returns new morphodata object
newMorphodata <- function() {
  newMorphodata <- list(ID = NULL, Population = NULL, Taxon = NULL, data = data.frame())
  attr(newMorphodata, "class") <- "morphodata"
  return(newMorphodata)
}


# internal
# returns new pcadata object
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
