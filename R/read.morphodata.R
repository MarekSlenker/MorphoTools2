#' Data Input
#'
#' @description Imports data from tab-delimited text file and produces a 'morphodata' object from it.
#'
#' @usage read.morphodata(infile, dec=".", ...)
#'
#' @param infile the name of the file which the data are to be read from.
#' @param dec the character used in the file for decimal points.
#' @param ... 	further arguments to be passed to read.table.
#'
#' @return object of class 'morphodata'
#'
#' @examples
#' read.morphodata("infile.txt")
#' read.morphodata("clipboard")
#' @export
read.morphodata<-function(infile, dec=".", ...){
  xdata<-read.delim(infile, header=T, dec = dec, ...)

  if (dim(xdata)[2] <= 3) stop("incorrect data format", call. = F)

  return(morphodata(xdata))
}
