#' Descriptive statistics
#'
#' @description These functions calculate the descriptive statistics of each character in the whole dataset,
#'  each taxon and each population.
#'
#' @usage descr.tax(object, format)
#'
#' @param object 	an object of class 'morphodata'.
#' @param format form to which will be formated descriptive characters. See Details.
#'
#' @details the following statistics are computed: number of observations, mean, standard deviation, and the percentiles 0%
#'  (minimum), 5%, 25% (lower quartile), 50% (median), 75% (upper quartile), 95% and 100% (maximum).
#'
#' @details The "format" argument brings handy way how to receive only what is wanted and in format what is desired.
#' Othervays, if format remains NULL, output table is overwhelmed by all calculated descriptors. The format argument is single string,
#' where keywords will be replaced by  particular values.
#' @details Keywords: "$MEAN" = mean; "$SD" = standard deviation"; "$MIN" = minimum; "$5\%" =  5th percentile;
#' "$25\%" = 25th percentile (lower quartile); "$MEDIAN" = median (50th percentile); "$75\%" = 75th percentile (upper quartile);
#' "$95\%" = 95th percentile; "$MAX" = maximum.
#'
#'
#' @examples
#' descr.tax(object, format = "($MEAN +- $SD)")
#'
#' @export
descr.tax <- function(object, format = NULL) {
  checkMorphodataClass(object)

  # calculate descr stat
  descrStatistic = descrByGroup(object, "Taxon")

  taxa =  levels(object$Taxon)
  characters = colnames(object$data)

  if ( !(is.null(format))){
    # format output according to user specification
    return(formatDescrStatistic(taxa, characters, descrStatistic, format))
  }
  else{
    # do not format descr stat for export ~ row data
    return(unFormatDescrStatistic(taxa, characters, descrStatistic, format))
  }
}

#' @rdname descr.tax
#' @usage descr.pop(object, format)
#' @examples
#'
#' descr.pop(object, format = "$MEAN ($MIN - $MAX)")
#' @export
descr.pop <- function(object, format = NULL) {
  checkMorphodataClass(object)

  # calculate descr stat
  descrStatistic = descrByGroup(object, "Population")

  populs =  levels(object$Population)
  characters = colnames(object$data)

  if ( !(is.null(format))){
    # format output according to user specification
    return(formatDescrStatistic(populs, characters, descrStatistic, format))
  }
  else{
    # do not format descr stat for export ~ row data
    return(unFormatDescrStatistic(populs, characters, descrStatistic, format))
  }
}

#' @rdname descr.tax
#' @usage descr.all(object, format)
#' @examples
#'
#' descr.all(object, format = "$MEAN ± $SD ($5% - $95%)")
#' @export
descr.all <- function(object, format = NULL) {
  checkMorphodataClass(object)

  # calculate descr stat
  descrStatistic = descrByGroup(object, "Population")

  if ( !(is.null(format))){
    # format output according to user specification
    return(formatDescrStatistic(object, descrStatistic, format))
  }
  else{
    # do not format descr stat for export ~ row data
    return(unFormatDescrStatistic(object, descrStatistic, format))
  }
}


