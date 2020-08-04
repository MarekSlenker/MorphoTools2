

#' @export
print.classifdata <- function(x, ...) {

  toPrint = classif.matrix(x, level = "indiv")


  print(toPrint, ...)
}

#' @export
print.morphodata <- function(x, ...) {

  objToPrint = data.frame("ID" = x$ID, "Population" = x$Population, "Taxon" = x$Taxon, x$data)

  row.names(objToPrint) = NULL

  print(objToPrint, ...)
}




#' @importFrom utils head
#' @export
head.classifdata <- function(x, n = 6, ...) {
  toPrint = classif.matrix(x, level = "indiv")

  utils::head(toPrint, n = n, ...)
}

#' @importFrom utils tail
#' @export
tail.classifdata <- function(x, n = 6, ...) {
  toPrint = classif.matrix(x, level = "indiv")

  utils::tail(toPrint, n = n, ...)
}




#' @importFrom utils head
#' @export
head.morphodata <- function(x, n = 6, ...) {

  objToPrint = data.frame("ID" = x$ID, "Population" = x$Population, "Taxon" = x$Taxon, x$data)

  row.names(objToPrint) = NULL

  utils::head(objToPrint, n = n, ...)
}

#' @importFrom utils tail
#' @export
tail.morphodata <- function(x, n = 6, ...) {

  objToPrint = data.frame("ID" = x$ID, "Population" = x$Population, "Taxon" = x$Taxon, x$data)

  row.names(objToPrint) = NULL

  utils::tail(objToPrint, n = n, ...)
}

