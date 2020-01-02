# generic function


#' Add labels to a plot
#'
#' @description This is a generic function for drawing labels to the data points.
#'
#' @usage labels.points(result, axes = c(1,2), ...)
#' @usage labels.characters(result, axes = c(1,2), ...)
#'
#' @param result 	an results previously ploted.
#' @param axes x, y axes of plot
#' @param ... further arguments to be passed to \code{\link{text}} or other graphical parameters in \code{\link{par}}.
#'
#' @export
labels.points <- function(object, ...) {
  UseMethod("labels.points")
}


#' @rdname pca.calc
#' @usage NULL
#' @method labels.points pcadata
#' @export
labels.points.pcadata <- function(object, axes = c(1,2), ...) {
  checkClass(object, "pcadata")

  labels.points.internal(object, axes, ...)

}


# suitable for "pcadata" or "cdadata", as both stores XY coordinates in $scores
labels.points.internal <- function(object, axes, ...) {
  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (max(axes) > length(object$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(object$eigenValues), " axes.", sep = "" ), call. = F)

  text(x = object$objects$scores[ ,axes[1]], y = object$objects$scores[ ,axes[2]],
       labels = object$objects$ID, ...)

}
