
# constructors

# internal
# returns new morphodata object
.newMorphodata <- function() {
  .newMorphodata <- list(
                      ID = NULL,
                      Population = NULL,
                      Taxon = NULL,
                      data = data.frame()
                      )

  attr(.newMorphodata, "class") <- "morphodata"
  return(.newMorphodata)
}


# internal
# returns new pcadata object
.newPcadata <- function() {

  .newPcadata <- list(
                     objects = list(
                          ID = numeric(),
                          Population = numeric(),
                          Taxon = numeric(),
                          scores = numeric()),
                     eigenvectors = numeric(),
                     eigenvalues = numeric(),
                     eigenvaluesAsPercentages = numeric(),
                     cumulativePercentageOfEigenvalues = numeric(),
                     groupMeans = data.frame(),
                     rank = numeric(),
                     center = numeric(),
                     scale = numeric()
                     )

  attr(.newPcadata, "class") <- "pcadata"
  return(.newPcadata)
}

# internal
# returns new pcoadata object
.newPcoadata <- function() {

  .newPcoadata <- list(
    objects = list(
      ID = numeric(),
      Population = numeric(),
      Taxon = numeric(),
      scores = numeric()),
    eigenvalues = numeric(),
    eigenvaluesAsPercentages = numeric(),
    cumulativePercentageOfEigenvalues = numeric(),
    groupMeans = data.frame(),
    distMethod = character(),
    rank = numeric()
  )

  attr(.newPcoadata, "class") <- "pcoadata"
  return(.newPcoadata)
}


# internal
# returns new nmdsdata object
.newNmdsdata <- function() {

  .newNmdsdata <- list(
    objects = list(
      ID = numeric(),
      Population = numeric(),
      Taxon = numeric(),
      scores = numeric()),
    stress = numeric(),
    #diss = numeric(),
    #dist = numeric(),
    #eigenvectors = numeric(),
    #eigenvalues = numeric(),
    #eigenvaluesAsPercentages = numeric(),
    #cumulativePercentageOfEigenvalues = numeric(),
    groupMeans = data.frame(),
    distMethod = character(),
    rank = numeric()
  )

  attr(.newNmdsdata, "class") <- "nmdsdata"
  return(.newNmdsdata)
}


# internal
# returns new cdadata object
.newCdadata <- function() {

  .newCdadata <- list(
    objects = list(
      ID = factor(),
      Population = factor(),
      Taxon = factor(),
      scores = data.frame()),
    eigenvalues = numeric(),
    eigenvaluesAsPercentages = numeric(),
    cumulativePercentageOfEigenvalues = numeric(),
    groupMeans = data.frame(),
    rank = numeric(),
    coeffs.std = matrix(),
    coeffs.raw = matrix(),
    totalCanonicalStructure = matrix(),
    canrsq = numeric()
    )

  attr(.newCdadata, "class") <- "cdadata"
  return(.newCdadata)
}


# internal
# returns new classifdata object
.newClassifdata <- function() {
  .newClassifdata <- list(
    ID = character(),
    Population = factor(),
    Taxon = factor(),
    classif = factor(),
    prob = numeric(),
    correct = logical()

  )

  attr(.newClassifdata, "class") <- "classifdata"
  return(.newClassifdata)
}

