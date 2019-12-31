
#' @rdname pca.calc
#' @usage
#' ## S3 method for class 'pcadata'
#' plot(pcaResult, axes = c(1,2), pch = 16, col = "black", ...)
#'
#' @param pcaResult object of class 'pcadata'
#'
#' @examples
#'
#'
#' @export

plot.pcadata <- function(object, axes = c(1,2), xlab = NULL, ylab = NULL,
                         pch = 16, col = "black", ...) {
  checkClass(object, "pcadata")

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (max(axes) > length(object$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(object$eigenValues), " axes.", sep = "" ), call. = F)

  if (is.null(xlab))
    xlab = paste("PC",axes[1], " (", round(object$axesVariance[axes[1]]*100, digits = 2) ,"%)", sep = "")

  if (is.null(ylab))
    ylab = paste("PC",axes[2], " (", round(object$axesVariance[axes[2]]*100, digits = 2) ,"%)", sep = "")


  # nastav pch a col spravne podla taxonu
  object$pch = as.numeric( setValuesForVector(object$objects$Taxon, pch))

  object$col = setValuesForVector(object$objects$Taxon, col)


  # main plot
  plot(x = object$objects$scores[ ,axes[1]], y = object$objects$scores[ ,axes[2]],
       xlab = xlab, ylab = ylab, pch = object$pch, col = object$col, ... )

}
