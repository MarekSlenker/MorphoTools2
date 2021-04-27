#' Population means
#' @export
populOTU <- function(object) {
  .checkClass(object, "morphodata")

  populData = stats::aggregate(object$data, by =  list( object$Taxon, object$Population), mean, na.rm=TRUE)

  populData[, -c(1:2)] = data.frame(sapply(populData[, -c(1:2)], function(x) ifelse(is.nan(x), NA, x)))

  if (any(is.na(populData[, -c(1:2)]))) {
    naChars = colnames(populData)[apply(populData, 2, function(x) any(is.na(x)))]
    naPops = populData$Group.2[apply(populData, 1, function(x) any(is.na(x)))]
    warning(paste("Unable to calculate the means of characters ", paste(naChars, collapse = " "), " in populations ", paste(naPops, collapse = " "), ". Values are NA.", sep = ""), call. = FALSE)
  }

  dt = data.frame("ID" = populData[,2], "Population" = populData[,2],
                  "Taxon" = populData[,1], populData[ ,-c(1:2)])

  return(.morphodataFromDataFrame(dt))
}
