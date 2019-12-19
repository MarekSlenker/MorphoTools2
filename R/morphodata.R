# Class "morphodata"
morphodata <- function(indata) {

  # check for required columns
  if(!("ID" %in% colnames(indata)) | !("Population" %in% colnames(indata)) | !("Taxon" %in% colnames(indata)))
    stop("input do not contains required columns", call. = F)


  data = list(
    "ID" = as.factor(indata$ID),
    "Population" = as.factor(indata$Population),
    "Taxon" = as.factor(indata$Taxon),
    "data" = as.data.frame(indata[,-(1:3)],  row.names = as.character(indata$ID))
    #, row.names = indata$ID
  )

  #testuj ci tam nie je nejaky nezmysel .. slovo v cislach etc   .  cislo ako pop?
  if (!(is.numeric(as.matrix(data$data)))) stop("input contains non-numeric data", call. = F)

  attr(data, "class") <- "morphodata"
  return(data)
}
