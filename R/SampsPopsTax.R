

#' @rdname read.morphodata
#' @export
samples <- function(object) {
  checkClass(object, "morphodata")

  return(levels(object$ID))
}


#' @rdname read.morphodata
#' @export
populations <- function(object) {
  checkClass(object, "morphodata")

  return(levels(object$Population))
}


#' @rdname read.morphodata
#' @export
taxa <- function(object) {
  checkClass(object, "morphodata")

  return(levels(object$Taxon))
}
