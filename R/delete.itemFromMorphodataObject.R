#' Remove items (Taxa, Populations, morphological characters) from an morphodata object
#'
#' @description This functions remove particular taxa, populations or morphological characters from morphodata object.
#'
#' @usage delete.taxon(object, taxonName)
#'
#' @param object an object of class 'morphodata'.
#' @param taxonName vector of taxa to be removed.
#'
#' @return object of class 'morphodata'
#'
#' @examples
#' myPartialDataset = delete.taxon(initialdataset, "hybr")
#' myPartialDataset = delete.taxon(initialdataset, c("ph", "st"))
#'
#' @export
delete.taxon <- function(object, taxonName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma taxName
  for (tax in taxonName) {
    if (! (tax %in% object$Taxon)) stop(paste("taxon", tax , "does not exist"), call. = F)
  }

  return(removeByColumn(object, "Taxon", taxonName))
}


#' @rdname delete.taxon
#' @usage delete.population(object, populationName)
#'
#' @param populationName vector of populations to be removed.
#'
#' @examples
#' myPartialDataset = delete.population(initialdataset, "CZLE")
#' myPartialDataset = delete.population(initialdataset, c("CERV", "DEB", "KOT"))
#'
#' @export
delete.population <- function(object, populationName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma popname
  for (pop in populationName) {
    if (! (pop %in% object$Population)) stop(paste("population", pop , "does not exist"), call. = F)
  }

  return(removeByColumn(object, "Population", populationName))
}


#' @rdname delete.taxon
#' @usage delete.charecter(object, charecterName)
#'
#' @param charecterName vector of characters to be removed.
#'
#' @examples
#' myPartialDataset = delete.charecter(initialdataset, "StemHeight")
#' myPartialDataset = delete.charecter(initialdataset, c("StemHeight", "LeafNo", "PetalLength"))
#'
#' @export
delete.charecter <- function(object, charecterName) {
  checkClass(object, "morphodata")

  # check existence of CH
  for (ch in charecterName) {
    if (! (ch %in% colnames(object$data))) stop(paste("charecter", ch , "does not exist"), call. = F)
  }

  # charecter - moze byt i viac
  toRemove = array(data = NA, dim = 0)
  for (ch in charecterName) {
    toRemove = c(toRemove, which(colnames(object$data) == ch) )
  }

  object$data = object$data[ ,-toRemove]

  return(object)
}


# internal
# @param object object of class morphodata
# @param column column where to look for groupName
# @param groupName name of particular Population or Taxon, which should be removed

removeByColumn <- function(object, column, groupName) {
  # obj je triedy morfodata, skontrolovane vyssie

  # groupName moze byt i viac
  toRemove = array(data = NA, dim = 0)
  for (name in groupName) {
    toRemove = c(toRemove, which( unlist(object[column]) %in% groupName) )
  }

  newObject = newMorphodata()
  newObject$ID = droplevels( object$ID[-toRemove] )
  newObject$Population = droplevels( object$Population[-toRemove] )
  newObject$Taxon = droplevels( object$Taxon[-toRemove] )
  newObject$data = object$data[-toRemove, ]

  return(newObject)
}

keepByColumn <- function(object, column, groupName) {
  # obj je triedy morfodata, skontrolovane vyssie

  # groupName moze byt i viac
  toKeep = array(data = NA, dim = 0)
  for (name in groupName) {
    toKeep = c(toKeep, which( unlist(object[column]) %in% groupName) )
  }

  newObject = newMorphodata()
  newObject$ID = droplevels( object$ID[toKeep] )
  newObject$Population = droplevels( object$Population[toKeep] )
  newObject$Taxon = droplevels( object$Taxon[toKeep] )
  newObject$data = object$data[toKeep, ]

  return(newObject)
}




