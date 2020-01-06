# generic function


#' Draws eigenvectors as arrows
#'
#' @description The correlation of the characters and ordination axes are visualised as arrows.
#'
#' @usage plot.characters(pcaResult, axes = c(1,2), col = "red", length = 0.1, labels = TRUE, ...)
#'
#' @param pcaResult object of class 'pcadata'.
#' @param axes x, y axes of plot.
#' @param col the color of arro ws.
#' @param length length of the edges of the arrow head (in inches).
#' @param labels logical, defines if labels are displayed. Only restricted number of parameters are supported. For more precise labels ploting, use \code{\link{labels.characters}} directly.
#' @param ... further arguments to be passed to \code{\link{arrows}} or other graphical parameters in \code{\link{par}}.
#'
#' @export
plot.characters <- function(pcaResult, ...) {
  UseMethod("plot.characters")
}


#' @rdname pca.calc
# USAGE je v    plot.points.pcadata
#' @usage NULL
#' @method plot.characters pcadata
#' @export
plot.characters.pcadata <- function(pcaResult, axes = c(1,2), xlab = NULL, ylab = NULL,
                            col = "red", length = 0.1, labels = TRUE, ...) {
  checkClass(pcaResult, "pcadata")

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (max(axes) > length(pcaResult$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(pcaResult$eigenValues), " axes.", sep = "" ), call. = F)

  if (is.null(xlab))
    xlab = paste("PC",axes[1], " (", round(pcaResult$axesVariance[axes[1]]*100, digits = 2) ,"%)", sep = "")

  if (is.null(ylab))
    ylab = paste("PC",axes[2], " (", round(pcaResult$axesVariance[axes[2]]*100, digits = 2) ,"%)", sep = "")

  # main plot

  plot(x = pcaResult$eigenVectors[ ,axes[1]], y = pcaResult$eigenVectors[ ,axes[2]],
         type = "n", xlab = xlab, ylab = ylab, ...)
  abline(h = 0,v = 0,lty = 2,col = "grey")
  arrows(0, 0, pcaResult$eigenVectors[ ,axes[1]], pcaResult$eigenVectors[ ,axes[2]],
         col = col, length = length, ...)

  if (labels == T) text(x = pcaResult$eigenVectors[ ,axes[1]], y = pcaResult$eigenVectors[ ,axes[2]],
                      labels = row.names(pcaResult$eigenVectors), cex = 0.7, pos = 4, offset = 0.5)


}
