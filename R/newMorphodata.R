# internal
# returns new morphodata object


newMorphodata <- function() {
  newMorphodata <- list(ID = NULL, Population = NULL, Taxon = NULL, data = data.frame())
  attr(newMorphodata, "class") <- "morphodata"
  return(newMorphodata)
}
