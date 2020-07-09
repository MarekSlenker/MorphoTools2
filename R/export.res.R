# generic function

#' Export data
#' @export
export.res <- function(object, file = "clipboard", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {
  UseMethod("export.res")
}

#' @rdname export.res
#' @export
export.res.morphodata <- function(object, file = "clipboard", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {
  checkClass(object, "morphodata")

  objToWrite = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon, object$data)

  export.res(objToWrite, file = file, dec = dec, sep = sep, row.names = row.names, col.names = col.names)
}


#' @rdname export.res
#' @export
export.res.data.frame <- function(object, file = "clipboard", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

  if (!(is.data.frame(object))) stop("object is not of class 'data.frame'", call. = F)

  utils::write.table(object, file = file, dec = dec, sep = sep, quote = F, row.names = row.names, col.names = col.names, na = "")
}



#  DOLE  NETESTOVANE


#' @rdname export.res
#' @export
export.res.matrix <- function(object, file = "clipboard", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

  if (!(is.matrix(object))) stop("object is not of class 'matrix'", call. = F)

  objToWrite = data.frame("character" = rownames(object), object)

  export.res(objToWrite, file = file, dec = dec, sep = sep, row.names = row.names, col.names = col.names)
}


#' @rdname export.res
#' @export
export.res.numeric <- function(object, file = "clipboard", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

  if (!(is.numeric(object))) stop("object is not of class 'numeric'", call. = F)

  objToWrite = as.data.frame(object)

  export.res(objToWrite, file = file, dec = dec, sep = sep, row.names = FALSE, col.names = FALSE)
}


#' @rdname export.res
#' @export
export.res.classifdata <- function(object, file = "clipboard", dec = ".", sep = "\t", row.names = FALSE, col.names = TRUE) {

  checkClass(object, "classifdata")

  attr(object, "class") <- "data.frame"

  export.res(object, file = file, dec = dec, sep = sep, row.names = row.names, col.names = col.names)
}




