#' Remove population from an morphodata object
#'
#' @description This function remove particular population / populations from morphodata object.
#'
#' @usage delete.population(object, populationName)
#'
#' @param object an object of class 'morphodata'.
#' @param populationName vector of populations to be removed.
#'
#' @return object of class 'morphodata'
#'
#' @examples
#' myPartialDataset = delete.population(initialdataset, "CZLE")
#' myPartialDataset = delete.population(initialdataset, c("CERV", "DEB", "KOT"))

#' @export

delete.population <- function(object, populationName) {
  checkMorphodataClass(object)

  # skontroluj ci object ma popname
  for (pop in populationName) {
    if (! (pop %in% object$Population)) stop(paste("population", pop , "does not exist"), call. = F)
  }

  return(removeByColumn(object, "Population", populationName))
}
