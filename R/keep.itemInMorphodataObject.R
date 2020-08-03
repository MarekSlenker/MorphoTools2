#' Keep items (Taxa, Populations, morphological characters) in an morphodata object (and remove other)
#' @export
keepTaxon <- function(object, taxonName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma taxName
  for (tax in taxonName) {
    if (! (tax %in% object$Taxon)) stop(paste("taxon", tax , "does not exist"), call. = F)
  }

  return(keepByColumn(object, "Taxon", taxonName))
}


#' @rdname keepTaxon
#' @export
keepPopulation <- function(object, populationName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma popname
  for (pop in populationName) {
    if (! (pop %in% object$Population)) stop(paste("population", pop , "does not exist"), call. = F)
  }

  return(keepByColumn(object, "Population", populationName))
}


#' @rdname keepTaxon
#' @export
keepCharecter <- function(object, charecterName) {
  checkClass(object, "morphodata")

  # check existence of CH
  for (ch in charecterName) {
    if (! (ch %in% colnames(object$data))) stop(paste("charecter", ch , "does not exist"), call. = F)
  }

  # charecter - moze byt i viac
  toKeep = array(data = NA, dim = 0)
  for (ch in charecterName) {
    toKeep = c(toKeep, which(colnames(object$data) == ch) )
  }

  if (length(toKeep)>1) {
    object$data = object$data[ ,toKeep]
  } else {
    object$data = data.frame(object$data[ ,toKeep])
    colnames(object$data) = charecterName
  }


  return(object)
}


# internal
# @param object object of class morphodata
# @param column column where to look for groupName
# @param groupName name of particular Population or Taxon, which should be keeped
# internal
keepByColumn <- function(object, column, groupName) {
  # obj je triedy morfodata, skontrolovane vyssie

  # groupName moze byt i viac
  toKeep = array(data = NA, dim = 0)
  for (name in groupName) {
    toKeep = c(toKeep, which( unlist(object[column]) %in% name) )
  }

  newObject = newMorphodata()
  newObject$ID = droplevels( object$ID[toKeep] )
  newObject$Population = droplevels( object$Population[toKeep] )
  newObject$Taxon = droplevels( object$Taxon[toKeep] )
  newObject$data = object$data[toKeep, ]

  return(newObject)
}


