

#' @export
print.classifdata <- function(object) {
  checkClass(object, "classifdata")

  attr(object, "class") <- "data.frame"

  print(object)
}

#' @export
head.classifdata <- function(object, n = 6, ...) {
  checkClass(object, "classifdata")

  attr(object, "class") <- "data.frame"

  head(object, n = n, ...)
}

#' @export
tail.classifdata <- function(object, n = 6, ...) {
  checkClass(object, "classifdata")

  attr(object, "class") <- "data.frame"

  tail(object, n = n, ...)
}

#' @export
print.morphodata <- function(object) {
  checkClass(object, "morphodata")

  objToPrint = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon, object$data)

  row.names(objToPrint) = NULL

  print(objToPrint)
}

#' @export
head.morphodata <- function(object, n = 6, ...) {
  checkClass(object, "morphodata")

  objToPrint = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon, object$data)

  row.names(objToPrint) = NULL

  head(objToPrint, n = n, ...)
}

#' @export
tail.morphodata <- function(object, n = 6, ...) {
  checkClass(object, "morphodata")

  objToPrint = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon, object$data)

  row.names(objToPrint) = NULL

  tail(objToPrint, n = n, ...)
}

