#' Data input
#' @export
read.morphodata <- function(infile, dec = ".", sep = "\t", ...){
  data = read.delim(infile, header = T, dec = dec, sep = sep, ...)

  if (dim(data)[2] <= 3) stop("incorrect data format", call.=F)

  return(morphodataFromDataFrame(data))
}
