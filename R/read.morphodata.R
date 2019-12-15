#' A read.morphodata Function
#'
#' @description Imports data from tab-delimited text file and produces a "morphodata" object from it.
#' @usage read.morphodata(infile, dec=".")
#' @param infile the name of the file which the data are to be read from.
#' @param dec the character used in the file for decimal points.
#' @return Abject of class "morphodata"
#' @keywords
#' @export
#' @examples
#' read.morphodata("infile.txt")
#' read.morphodata("clipboard")
read.morphodata<-function(infile, dec="."){
  xdata<-read.delim(infile, header=T, dec = dec)

  if (dim(xdata)[2] <= 3) stop("incorrect data format", call. = F)

  return(morphodata(xdata))



  }
