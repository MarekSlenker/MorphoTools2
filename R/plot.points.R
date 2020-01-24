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
#' plot.points(pcaResult, axes = c(1,2), pch = 16, col = "black", labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...)
#'
#' ## S3 method for class 'pcadata'
#' plot.3D(pcaResult, axes = c(1,2,3), pch = 16, col = "black", ???? ?????? ??????????????????? ?????????????????????????? labels = FALSE ????????????????????????????????????????? , legend = FALSE, legend.pos = "topright", ncol = 1, ...)
#'
#' ## S3 method for class 'pcadata'
#' plot.characters(pcaResult, axes = c(1,2), col = "red", length = 0.1, xlim = c(-1,1), ylim = c(-1,1), labels = TRUE. ...)
#'
#' ## S3 method for class 'pcadata'
#' labels.points(pcaResult, axes = c(1,2), ...)
#'
#' @param pcaResult object of class 'pcadata'.
#' @param axes x, y axes of plot.
#' @param pch a vector of plotting characters or symbols: see \code{\link{points}}.
#' @param col the colors for points. Multiple colors can be specified so that each Taxon can be given its own color. If there are fewer colors than points they are recycled in the standard fashion.
#' @param labels logical, defines if labels are displayed. Only restricted number of parameters are supported. For more precise labels ploting, use \code{\link{labels.points}} directly.
#' @param legend logical, defines if legend is displayed. Only restricted number of legend parameters are supported. For more precise legend ploting, use \code{\link{plot.legend}} directly.
#' @param legend.pos the x and y co-ordinates or a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.
#' @param ncol the number of columns in which to set the legend items (default is 1, a vertical legend).
#' @param length length of the edges of the arrow head (in inches).
#' @param ... further arguments to be passed to \code{\link{plot.default}}, \code{\link{text}}, \code{\link{arrows}} or other graphical parameters in \code{\link{par}}.
#'
#' @examples
#' plot.points(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "red"), pch = c(20, 17, 8, 21), bg = "orange", legend = T, legend.pos = "bottomright", ncol = 2)
#' plot.points(pcaRes, main = "My PCA plot", frame.plot = F, cex = 0.8)
#'
#' plot.3D(_________________________________________________________________)
#'
#' plot.characters(pcaRes, axes = c(1,2))
#'
#' labels.points(pcaRes, axes = c(1,2), cex = 0.8, col = "red", pos = 4, offset = 0.4)
#'
#' @method plot.points pcadata
#' @export
plot.points.pcadata <- function(pcaResult, axes = c(1,2), xlab = NULL, ylab = NULL,
                         pch = 16, col = "black", bg = "white", labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...) {
  checkClass(pcaResult, "pcadata")

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (max(axes) > length(pcaResult$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(pcaResult$eigenValues), " axes.", sep = "" ), call. = F)

  if (is.null(xlab))
    xlab = paste("PC",axes[1], " (", round(pcaResult$axesVariance[axes[1]]*100, digits = 2) ,"%)", sep = "")

  if (is.null(ylab))
    ylab = paste("PC",axes[2], " (", round(pcaResult$axesVariance[axes[2]]*100, digits = 2) ,"%)", sep = "")


  # nastav pch a col spravne podla taxonu
  pcaResult$pch = as.numeric( setValuesForVector(pcaResult$objects$Taxon, pch))

  pcaResult$col = setValuesForVector(pcaResult$objects$Taxon, col)


  # main plot

  plot(x = pcaResult$objects$scores[ ,axes[1]], y = pcaResult$objects$scores[ ,axes[2]],
       xlab = xlab, ylab = ylab, pch = pcaResult$pch, col = pcaResult$col, bg = bg, ... )


  if (legend == TRUE) {
    if (length(legend.pos) == 1) legend(legend.pos,
                                        legend = unique(pcaResult$objects$Taxon),
                                        pch = unique(pcaResult$pch),
                                        col = unique(pcaResult$col),
                                        bty="o", pt.bg = bg, ncol = ncol)
    else legend(legend.pos[1], legend.pos[2], legend = unique(pcaResult$objects$Taxon),
                pch = unique(pcaResult$pch),
                col = unique(pcaResult$col),
                bty="o", pt.bg = bg, ncol = ncol)}

  if (labels == T) text(x = pcaResult$objects$scores[ ,axes[1]], y = pcaResult$objects$scores[ ,axes[2]],
                        labels = pcaResult$objects$ID, cex = 0.7, pos = 4, offset = 0.5)


}