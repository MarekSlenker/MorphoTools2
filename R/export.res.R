# generic function


#' Export data
#'
#' @description This is a generic function for exporting results, stored in objects of MorphoTools package.
#'
#' @usage export.res(object, ...)
#'
#' @param object 	an object to be exported.
#' @param ... further arguments passed to or from other methods.
#' @param file either a character string naming a file or a connection open for writing. "" indicates output to the console.
#' @param dec the string to use for decimal points in numeric or complex columns: must be a single character.
#' @param sep the field separator string. Values within each row of x are separated by this string.
#'
#' @examples
#' myCompleteDataset = na.meansubst(initialdataset)
#' @export
export.res <- function(object, ...) {
  UseMethod("export.res")
}


#' @describeIn export.res function for exporting 'morphodata' objects
#' @export
export.res.morphodata <- function(object, file = "clipboard", dec = ".", sep = "\t") {
  checkMorphodataClass(object)

  objToWrite = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon, object$data)

  write.table(objToWrite, file = file, dec = dec, sep = sep, quote = F, row.names = F, col.names = T, na = "")
}


