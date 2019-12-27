#' Data input
#'
#' @description Imports data from tab-delimited text file and produces a 'morphodata' object from it.
#'
#' @usage read.morphodata(infile, dec=".", ...)
#'
#' @param infile the name of the file which the data are to be read from.
#' @param dec the character used in the file for decimal points.
#' @param sep the column separator character.
#' @param ... 	further arguments to be passed to read.table.
#'
#' @return object of class 'morphodata'
#'
#' @details
#' The function expect the following data structure:
#' (1) the first row contains variable names.
#' (2) the following rows contains individuals, single individual per row;
#' (3) the first three columns include unique identifiers for individuals, populations and taxa/groups, respectively. Columns have to be named as “ID”, “Population” and “Taxon”;
#' (4) starting from the fourth column, any number of quantitative or binary morphological characters may be recorded. Any column names can be used (avoiding spaces and special characters);
#'
#' If there are missing values in the data, they must be represented as empty cells or by the text NA (not quoted), not zero, space or any other character.
#' @examples
#' read.morphodata("infile.txt")
#' read.morphodata("clipboard")
#' @export
read.morphodata <- function(infile, dec=".", sep="\t", ...){
  data = read.delim(infile, header=T, dec=dec, sep=sep, ...)

  if (dim(data)[2] <= 3) stop("incorrect data format", call.=F)

  return(morphodata(data))
}
