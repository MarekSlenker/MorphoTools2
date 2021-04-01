# generic function

#' Export data
#' @export
exportRes <- function(object, file = "", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {
  UseMethod("exportRes")
}

#' @rdname exportRes
#' @export
exportRes.morphodata <- function(object, file = "", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

    objToWrite = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon, object$data)

  exportRes(objToWrite, file = file, dec = dec, sep = sep, row.names = row.names, col.names = col.names)
}


#' @rdname exportRes
#' @export
exportRes.data.frame <- function(object, file = "", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

  utils::write.table(object, file = file, dec = dec, sep = sep, quote = F, row.names = row.names, col.names = col.names, na = "")
}



#  DOLE  NETESTOVANE


#' @rdname exportRes
#' @export
exportRes.matrix <- function(object, file = "", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

  objToWrite = data.frame("character" = rownames(object), object)

  exportRes(objToWrite, file = file, dec = dec, sep = sep, row.names = row.names, col.names = col.names)
}


#' @rdname exportRes
#' @export
exportRes.numeric <- function(object, file = "", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

  objToWrite = as.data.frame(object)

  exportRes(objToWrite, file = file, dec = dec, sep = sep, row.names = FALSE, col.names = FALSE)
}


#' @rdname exportRes
#' @export
exportRes.classifdata <- function(object, file = "", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

  attr(object, "class") <- "data.frame"

  exportRes(object, file = file, dec = dec, sep = sep, row.names = row.names, col.names = col.names)
}




