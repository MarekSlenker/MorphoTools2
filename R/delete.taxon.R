#' Remove taxon from an morphodata object
#'
#' @description This function remove particular taxon / taxa from morphodata object.
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

#' @export

delete.taxon <- function(object, taxonName) {
  checkClass(object, "morphodata")

  # skontroluj ci object ma taxName
  for (tax in taxonName) {
    if (! (tax %in% object$Taxon)) stop(paste("taxon", tax , "does not exist"), call. = F)
  }

  return(removeByColumn(object, "Taxon", taxonName))
}
