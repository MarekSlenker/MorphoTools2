# generic function


#' Add labels to a 3D plot
#'
#' @description This is a generic function for drawing labels to the data points in 3D plots.
#'
#' @usage labels.points3D(result, axes = c(1,2,3), ...)
#'
#' @param result 	an results previously ploted.
#' @param axes x, y, z axes of plot
#' @param ... further arguments to be passed to \code{\link{text3D}} or other graphical parameters in \code{\link{par}}.
#'
#' @export
labels.points3D <- function(result, ...) {
  UseMethod("labels.points3D")
}


#' @rdname pca.calc
#' @usage NULL
#' @method labels.points3D pcadata
#' @export
labels.points3D.pcadata <- function(pcaResult, axes = c(1,2,3), ...) {
  checkClass(pcaResult, "pcadata")

  labels_points3D_internal(pcaResult, axes, ...)
}

#' @rdname cda.calc
#' @usage NULL
#' @method labels.points3D cdadata
#' @export
labels.points3D.cdadata <- function(cdaResult, axes = c(1,2,3), ...) {
  checkClass(cdaResult, "cdadata")

  labels_points3D_internal(cdaResult, axes, ...)
}


# suitable for "pcadata" or "cdadata", as both stores XY coordinates in $scores
labels_points3D_internal <- function(object, axes, phi = 0, theta = 0, ...) {
  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 3) stop("you have to specifi 3 axes (e.g., axes = c(1,2,3))", call. = F)

  if (object$rank == 1) stop("this method is not applicable to histograms", call. = F)
  if (max(axes) > object$rank) stop(paste("specified axes are out of bounds. Object has only ", object$rank, " axes.", sep = "" ), call. = F)

  text3D(x = object$objects$scores[ ,axes[1]], y = object$objects$scores[ ,axes[2]], z = object$objects$scores[ ,axes[3]],
         labels  = object$objects$ID, phi = phi, theta = theta, add = TRUE, ...)
}
