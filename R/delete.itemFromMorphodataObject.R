#' Remove items (Taxa, Populations, morphological characters) from an morphodata object
#' @export
deleteTaxon <- function(object, taxonName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma taxName
  for (tax in taxonName) {
    if (! (tax %in% object$Taxon)) stop(paste("taxon", tax , "does not exist"), call. = F)
  }

  return(removeByColumn(object, "Taxon", taxonName))
}


#' @rdname deleteTaxon
#' @export
deletePopulation <- function(object, populationName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma popname
  for (pop in populationName) {
    if (! (pop %in% object$Population)) stop(paste("population", pop , "does not exist"), call. = F)
  }

  return(removeByColumn(object, "Population", populationName))
}


#' @rdname deleteTaxon
#' @export
deleteCharecter <- function(object, charecterName) {
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

# internal
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




