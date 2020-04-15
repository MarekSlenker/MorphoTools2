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
labels.points <- function(result, ...) {
  UseMethod("labels.points")
}


#' @rdname pca.calc
#' @usage NULL
#' @method labels.points pcadata
#' @export
labels.points.pcadata <- function(result, axes = c(1,2), ...) {
  checkClass(result, "pcadata")

  labels_points_internal(result, axes, ...)
}

#' @rdname cda.calc
#' @usage NULL
#' @method labels.points cdadata
#' @export
labels.points.cdadata <- function(cdaResult, axes = c(1,2), ...) {
  checkClass(cdaResult, "cdadata")

  labels_points_internal(cdaResult, axes, ...)
}


# suitable for "pcadata" or "cdadata", as both stores XY coordinates in $scores
labels_points_internal <- function(object, axes, ...) {
  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (object$rank == 1) stop("this method is not applicable to histograms", call. = F)
  if (max(axes) > object$rank) stop(paste("specified axes are out of bounds. Object has only ", object$rank, " axes.", sep = "" ), call. = F)

  text(x = object$objects$scores[ ,axes[1]], y = object$objects$scores[ ,axes[2]],
       labels = object$objects$ID, ...)

}
