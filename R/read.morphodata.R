#' Data input
#' @export
read.morphodata <- function(file, dec = ".", sep = "\t", ...){
  data = utils::read.delim(file, header = TRUE, dec = dec, sep = sep, ...)

  if (dim(data)[2] <= 3) stop("Incorrect data format.", call. = FALSE)

  return(.morphodataFromDataFrame(data))
}


# Class "morphodata"
.morphodataFromDataFrame <- function(indata) {

  # check for required columns
  if(!("ID" %in% colnames(indata)) | !("Population" %in% colnames(indata)) | !("Taxon" %in% colnames(indata)))
    stop("Input do not contain required columns.", call. = FALSE)


  data = list(
    "ID" = as.factor(indata$ID),
    "Population" = as.factor(indata$Population),
    "Taxon" = as.factor(indata$Taxon),
    "data" = as.data.frame(indata[,-(1:3)],  row.names = as.character(indata$ID))
  )

  if (dim(indata)[2] == 4) {
    colnames(data$data) = colnames(indata)[4]
  }

  #testuj ci tam nie je nejaky nezmysel .. slovo v cislach etc   .  cislo ako pop?
  if (!(is.numeric(as.matrix(data$data)))) stop("Input contains non-numeric data.", call. = FALSE)

  attr(data, "class") <- "morphodata"
  return(data)
}
