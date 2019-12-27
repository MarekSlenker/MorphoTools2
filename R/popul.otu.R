#' Population means
#'
#' @description This function calculates the average value for each character in each population,
#' with the pairwise deletion of missing data.
#'
#' @usage popul.otu(object)
#'
#' @param object an object of class 'morphodata'.
#'
#' @return object of class 'morphodata'
#'
#' @details This function return morphodata object, where each population is used as the operational
#'  taxonomic unit, thus is represented by single row with average values for each character.
#' Note that when using populations as OTUs they are handled with the same weight in all analyses
#' (disregarding population size, within-population variation, etc.)
#'
#' @examples
#' populations = popul.otu(individuals)
#'
#' @export
popul.otu <- function(object) {
  checkMorphodataClass(object)

  populData = aggregate(object$data, by =  list( object$Taxon, object$Population), mean, na.rm=TRUE)

  populData = data.frame(sapply(populData, function(x) ifelse(is.nan(x), NA, x)))

  if (any(is.na(populData))) warning("Values of some characters are NA.", call. = FALSE)

  dt = data.frame("ID" = populData[,2], "Population" = populData[,2],
                  "Taxon" = populData[,1], populData[ ,-c(1:2)])

  return(morphodata(dt))
}
