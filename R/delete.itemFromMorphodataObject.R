#' Remove items (Taxa, Populations, morphological characters) from an morphodata object
#' @export
deleteTaxon <- function(object, taxonName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma taxName
  for (tax in taxonName) {
    if (! (tax %in% object$Taxon)) stop(paste("taxon", tax , "does not exist"), call. = FALSE)
  }

  return(removeByColumn(object, "Taxon", taxonName))
}


#' @rdname deleteTaxon
#' @export
deletePopulation <- function(object, populationName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma popname
  for (pop in populationName) {
    if (! (pop %in% object$Population)) stop(paste("population", pop , "does not exist"), call. = FALSE)
  }

  return(removeByColumn(object, "Population", populationName))
}

#' @rdname deleteTaxon
#' @export
deleteSample <- function(object, sampleName = NULL, missingPercentage = NA) {
  checkClass(object, "morphodata")

  # nemozu byt oba nenulova
  if (!is.na(missingPercentage) && !is.null(sampleName)) stop("Not implemented, use arguments 'sampleName' and 'missingPercentage' in separate runs.", call. = FALSE)

  if (!is.null(sampleName)) {

    if (!all(is.character(sampleName))) stop("'missingPercentage' is not a string", call. = FALSE)

    # skontroluj ci object ma popname
    for (samp in sampleName) {
      if (! (samp %in% object$ID)) stop(paste("sample", samp , "does not exist"), call. = FALSE)
    }

    return(removeByColumn(object, "ID", sampleName))
  }

  if (!is.na(missingPercentage)) {

    if (!is.numeric(missingPercentage)) stop("'missingPercentage' is not numeric.", call. = FALSE)

    aboveTreshold = rowMeans(is.na(object$data)) > missingPercentage

    newObject = newMorphodata()
    newObject$ID = droplevels( object$ID[!aboveTreshold] )
    newObject$Population = droplevels( object$Population[!aboveTreshold] )
    newObject$Taxon = droplevels( object$Taxon[!aboveTreshold] )
    newObject$data = object$data[!aboveTreshold, ]

    return(newObject)
  }

  # nemozu byt oba nulove
  if (is.na(missingPercentage) && is.null(sampleName)) stop("One of the arguments: 'sampleName' or 'missingPercentage' has to be specified.", call. = FALSE)


}


#' @rdname deleteTaxon
#' @export
deleteCharacter <- function(object, characterName) {
  checkClass(object, "morphodata")

  # check existence of CH
  for (ch in characterName) {
    if (! (ch %in% colnames(object$data))) stop(paste("character", ch , "does not exist"), call. = FALSE)
  }

  # character - moze byt i viac
  toRemove = array(data = NA, dim = 0)
  for (ch in characterName) {
    toRemove = c(toRemove, which(colnames(object$data) == ch) )
  }


  if (length(toRemove) == dim(object$data)[2]-1) {

    colName = colnames(object$data)[-toRemove]
    object$data = data.frame(object$data[ ,-toRemove])
    colnames(object$data) = colName

  } else {
    object$data = object$data[ ,-toRemove]
  }

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
    toRemove = c(toRemove, which( unlist(object[column]) %in% name) )
  }

  newObject = newMorphodata()
  newObject$ID = droplevels( object$ID[-toRemove] )
  newObject$Population = droplevels( object$Population[-toRemove] )
  newObject$Taxon = droplevels( object$Taxon[-toRemove] )
  newObject$data = object$data[-toRemove, ]

  return(newObject)
}




