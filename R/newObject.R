
# constructors

# internal
# returns new morphodata object
newMorphodata <- function() {
  newMorphodata <- list(ID = NULL, Population = NULL, Taxon = NULL, data = data.frame())
  attr(newMorphodata, "class") <- "morphodata"
  return(newMorphodata)
}


# internal
# returns new pcadata object
newPcadata <- function() {

  newPcadata <- list(sdev = numeric(),
                     center = numeric(),
                     scale = numeric(),
                     objects = list(
                          ID = numeric(),
                          Population = numeric(),
                          Taxon = numeric(),
                          scores = numeric()),
                     eigenVectors = numeric(),
                     eigenValues = numeric(),
                     axesVariance = numeric(),
                     cumulativeAxesVariance = numeric())

  attr(newPcadata, "class") <- "pcadata"
  return(newPcadata)
}


# Class "morphodata"
morphodataFromDataFrame <- function(indata) {

  # check for required columns
  if(!("ID" %in% colnames(indata)) | !("Population" %in% colnames(indata)) | !("Taxon" %in% colnames(indata)))
    stop("input do not contains required columns", call. = F)


  data = list(
    "ID" = as.factor(indata$ID),
    "Population" = as.factor(indata$Population),
    "Taxon" = as.factor(indata$Taxon),
    "data" = as.data.frame(indata[,-(1:3)],  row.names = as.character(indata$ID))
  )

  #testuj ci tam nie je nejaky nezmysel .. slovo v cislach etc   .  cislo ako pop?
  if (!(is.numeric(as.matrix(data$data)))) stop("input contains non-numeric data", call. = F)

  attr(data, "class") <- "morphodata"
  return(data)
}
