# Class "morphodata"
morphodata <- function(indata) {

  data = list(
    "ID" = as.factor(indata$ID),
    "Population" = as.factor(indata$Population),
    "Taxon" = as.factor(indata$Taxon),
    "data" = as.data.frame(indata[,-(1:3)])
  )

  #testuj ci tam nie je nejaky nezmysel .. slovo v cislach etc   .  cislo ako pop?
  if (!(is.numeric(as.matrix(dd$data)))) stop("input contains non-numeric data", call. = F)

  attr(data, "class") <- "morphodata"
  return(data)
}
