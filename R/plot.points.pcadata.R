# generic function


#' The default scatterplot function
#'
#' @description A generic function for ploting ordination scores stored in 'pcadata' and 'cdadata' objects.
#'
#' @usage plot.points(object, ...)
#'
#' @param object object of class 'pcadata' or 'cdadata'.
#' @param ... further arguments passed to or from other methods.
#' @export
plot.points <- function(object, ...) {
  UseMethod("plot.points")
}


#' @rdname pca.calc
#' @usage
#' ## S3 method for class 'pcadata'
#' plot.points(pcaResult, axes = c(1,2), pch = 16, col = "black", ...)
#'
#' ## S3 method for class 'pcadata'
#' labels.points(pcaResult, axes = c(1,2), ...)
#'
#' @param pcaResult object of class 'pcadata'.
#' @param axes x, y axes of plot.
#' @param pch a vector of plotting characters or symbols: see \code{\link{points}}.
#' @param col the colors for points. Multiple colors can be specified so that each Taxon can be given its own color. If there are fewer colors than points they are recycled in the standard fashion.
#' @param legend logical, defines if legend is displayed. Only restricted number of legend parameters are supported. For more precise legend ploting, use \code{\link{legend}} directly.
#' @param legend.pos the x and y co-ordinates or a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.
#' @param ncol the number of columns in which to set the legend items (default is 1, a vertical legend).
#' @param ... further arguments to be passed to \code{\link{plot.default}}, \code{\link{text}} or other graphical parameters in \code{\link{par}}.
#'
#' @examples
#' plot.points(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "red"), pch = c(20, 17, 8, 21), bg = "orange", legend = T, legend.pos = "bottomright", ncol = 2)
#' plot.points(pcaRes, main = "My PCA plot", frame.plot = F, cex = 0.8)
#'
#' labels.points(pcaRes, axes = c(1,2), cex = 0.8, col = "red", pos = 4, offset = 0.4)
#'
#' @method plot.points pcadata
#' @export
plot.points.pcadata <- function(object, axes = c(1,2), xlab = NULL, ylab = NULL,
                         pch = 16, col = "black", bg = "white", legend = FALSE, legend.pos = "topright", ncol = 2, ...) {
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
       xlab = xlab, ylab = ylab, pch = object$pch, col = object$col, bg = bg, ... )


  if (legend == TRUE) {
    if (length(legend.pos) == 1) legend(legend.pos,
                                        legend = unique(pcaRes$objects$Taxon),
                                        pch = unique(object$pch),
                                        col = unique(object$col),
                                        bty="o", pt.bg = bg, ncol = ncol)
    else legend(legend.pos[1], legend.pos[2], legend = unique(pcaRes$objects$Taxon),
                pch = unique(object$pch),
                col = unique(object$col),
                bty="o", pt.bg = bg, ncol = ncol)}


}
