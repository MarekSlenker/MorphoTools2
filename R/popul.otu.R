#' Population means
#' @export
popul.otu <- function(object) {
  checkClass(object, "morphodata")

  populData = aggregate(object$data, by =  list( object$Taxon, object$Population), mean, na.rm=TRUE)

  populData[, -c(1:2)] = data.frame(sapply(populData[, -c(1:2)], function(x) ifelse(is.nan(x), NA, x)))

  if (any(is.na(populData[, -c(1:2)]))) warning("Values of some characters are NA.", call. = FALSE)

  dt = data.frame("ID" = populData[,2], "Population" = populData[,2],
                  "Taxon" = populData[,1], populData[ ,-c(1:2)])

  return(morphodataFromDataFrame(dt))
}
