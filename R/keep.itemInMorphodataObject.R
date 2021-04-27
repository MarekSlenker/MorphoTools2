#' Keep items (Taxa, Populations, morphological characters) in an morphodata object (and remove other)
#' @export
keepTaxon <- function(object, taxonName) {
  .checkClass(object, "morphodata")

  # skontroluj ci object ma taxName
  for (tax in taxonName) {
    if (! (tax %in% object$Taxon)) stop(paste("taxon", tax , "does not exist"), call. = FALSE)
  }

  return(.keepByColumn(object, "Taxon", taxonName))
}


#' @rdname keepTaxon
#' @export
keepPopulation <- function(object, populationName) {
  .checkClass(object, "morphodata")

  # skontroluj ci object ma popname
  for (pop in populationName) {
    if (! (pop %in% object$Population)) stop(paste("population", pop , "does not exist"), call. = FALSE)
  }

  return(.keepByColumn(object, "Population", populationName))
}


#' @rdname keepTaxon
#' @export
keepSample <- function(object, sampleName = NULL, missingPercentage = NA) {
  .checkClass(object, "morphodata")

  # nemozu byt oba nenulova
  if (!is.na(missingPercentage) && !is.null(sampleName)) stop("Not implemented, use arguments 'sampleName' and 'missingPercentage' in separate runs.", call. = FALSE)


  if (!is.null(sampleName)) {

    if (!all(is.character(sampleName))) stop("'missingPercentage' is not a string", call. = FALSE)

    # skontroluj ci object ma popname
    for (samp in sampleName) {
      if (! (samp %in% object$ID)) stop(paste("sample", samp , "does not exist"), call. = FALSE)
    }

    return(.keepByColumn(object, "ID", sampleName))
  }

  if (!is.na(missingPercentage)) {

    if (!is.numeric(missingPercentage)) stop("'missingPercentage' is not numeric.", call. = FALSE)

    toKeep = rowMeans(is.na(object$data)) <= missingPercentage # ponecham tie, ktore maju max missingPercentage

    newObject = .newMorphodata()
    newObject$ID = droplevels( object$ID[toKeep] )
    newObject$Population = droplevels( object$Population[toKeep] )
    newObject$Taxon = droplevels( object$Taxon[toKeep] )
    newObject$data = object$data[toKeep, ]

    return(newObject)
  }

  # nemozu byt oba nulove
  if (is.na(missingPercentage) && is.null(sampleName)) stop("One of the arguments: 'sampleName' or 'missingPercentage' has to be specified.", call. = FALSE)


}


#' @rdname keepTaxon
#' @export
keepCharacter <- function(object, characterName) {
  .checkClass(object, "morphodata")

  # check existence of CH
  for (ch in characterName) {
    if (! (ch %in% colnames(object$data))) stop(paste("character", ch , "does not exist"), call. = FALSE)
  }

  # character - moze byt i viac
  toKeep = array(data = NA, dim = 0)
  for (ch in characterName) {
    toKeep = c(toKeep, which(colnames(object$data) == ch) )
  }

  if (length(toKeep)>1) {
    object$data = object$data[ ,toKeep]
  } else {
    object$data = data.frame(object$data[ ,toKeep])
    colnames(object$data) = characterName
  }


  return(object)
}


# internal
# @param object object of class morphodata
# @param column column where to look for groupName
# @param groupName name of particular Population or Taxon, which should be keeped
.keepByColumn <- function(object, column, groupName) {
  # obj je triedy morfodata, skontrolovane vyssie

  # groupName moze byt i viac
  toKeep = array(data = NA, dim = 0)
  for (name in groupName) {
    toKeep = c(toKeep, which( unlist(object[column]) %in% name) )
  }

  newObject = .newMorphodata()
  newObject$ID = droplevels( object$ID[toKeep] )
  newObject$Population = droplevels( object$Population[toKeep] )
  newObject$Taxon = droplevels( object$Taxon[toKeep] )
  newObject$data = object$data[toKeep, ]

  return(newObject)
}



