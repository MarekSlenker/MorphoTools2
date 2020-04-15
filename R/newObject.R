
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

  newPcadata <- list(
                     objects = list(
                          ID = numeric(),
                          Population = numeric(),
                          Taxon = numeric(),
                          scores = numeric()),
                     eigenVectors = numeric(),
                     eigenValues = numeric(),
                     axesVariance = numeric(),
                     cumulativeAxesVariance = numeric(),
                     groupMeans = data.frame(),
                     rank = numeric(),
                     sdev = numeric(),
                     center = numeric(),
                     scale = numeric()
                     )

  attr(newPcadata, "class") <- "pcadata"
  return(newPcadata)
}

# internal
# returns new pcadata object
newCdadata <- function() {

  newCdadata <- list(
    objects = list(
      ID = factor(),
      Population = factor(),
      Taxon = factor(),
      scores = data.frame()),
    eigenValues = numeric(),
    axesVariance = numeric(),
    cumulativeAxesVariance = numeric(),
    groupMeans = data.frame(),
    rank = numeric(),
    coeffs.std = matrix(),
    coeffs.raw = matrix(),
    totalCanonicalStructure = matrix(),
    canrsq = numeric()
    )

  attr(newCdadata, "class") <- "cdadata"
  return(newCdadata)
}


