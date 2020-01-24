# generic function


#' The default scatterplot function
#'
#' @description A generic function for ploting ordination scores stored in 'pcadata' and 'cdadata' objects.
#'
#' @usage plot.3D(object, ...)
#'
#' @param object object of class 'pcadata' or 'cdadata'.
#' @param ... further arguments passed to or from other methods.
#' @export
plot.3D <- function(object, ...) {
  UseMethod("plot.3D")
}


#' @rdname pca.calc
#' @param pcaResult object of class 'pcadata'.
#' @param axes x, y, z axes of plot.
#' @param pch a vector of plotting characters or symbols: see \code{\link{points}}.
#' @param col the colors for points. Multiple colors can be specified so that each Taxon can be given its own color. If there are fewer colors than points they are recycled in the standard fashion.
#' @param labels__?????__??????  logical, defines if labels are displayed. Only restricted number of parameters are supported. For more precise labels ploting, use \code{\link{labels.points}} directly.
#' @param legend logical, defines if legend is displayed. Only restricted number of legend parameters are supported. For more precise legend ploting, use \code{\link{plot.legend}} directly.
#' @param legend.pos the x and y co-ordinates or a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.
#' @param ncol the number of columns in which to set the legend items (default is 1, a vertical legend).
#' @param length length of the edges of the arrow head (in inches).
#' @param ... further arguments to be passed to \code{\link{plot.default}}, \code{\link{text}}, \code{\link{arrows}} or other graphical parameters in \code{\link{par}}.
#'
#' @method plot.3D pcadata
#' @export
plot.3D.pcadata <- function(pcaResult, axes = c(1,2,3), xlab = NULL, ylab = NULL, zlab = NULL,
                                pch = 16, col = "white", bg.col = "white",
                            phi = 10, theta = 2,
                            ticktype = "detailed", bty = "u", type = "n",
                            labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...) {
  checkClass(pcaResult, "pcadata")

  # skontroluj ci axes = 3; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 3) stop("you have to specifi 3 axes (e.g., axes = c(1,2,3))", call. = F)
  if (max(axes) > length(pcaResult$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(pcaResult$eigenValues), " axes.", sep = "" ), call. = F)

  if (is.null(xlab))
    xlab = paste("PC",axes[1], " (", round(pcaResult$axesVariance[axes[1]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(ylab))
    ylab = paste("PC",axes[2], " (", round(pcaResult$axesVariance[axes[2]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(zlab))
    zlab = paste("PC",axes[3], " (", round(pcaResult$axesVariance[axes[3]]*100, digits = 2) ,"%)", sep = "")


  # nastav pch a col spravne podla taxonu
  pcaResult$pch = as.numeric( setValuesForVector(pcaResult$objects$Taxon, pch))
  pcaResult$col = setValuesForVector(pcaResult$objects$Taxon, col)


  # main plot

  scatter3D(x = pcaResult$objects$scores[ ,axes[1]], y = pcaResult$objects$scores[ ,axes[2]], z = pcaResult$objects$scores[ ,axes[3]],
            colvar = NULL, colkey = T,
             col = pcaResult$col, pch = pcaResult$pch, bg.col = bg.col,
             phi = phi, theta = theta,
             ticktype = ticktype, bty = bty, type = type,
            xlab = xlab, ylab = ylab, zlab = zlab, ...)

  if (legend == TRUE) {
    if (length(legend.pos) == 1) legend(legend.pos,
                                        legend = unique(pcaResult$objects$Taxon),
                                        pch = unique(pcaResult$pch),
                                        col = unique(pcaResult$col),
                                        bty="o", pt.bg = bg.col, ncol = ncol)
    else legend(legend.pos[1], legend.pos[2], legend = unique(pcaResult$objects$Taxon),
                pch = unique(pcaResult$pch),
                col = unique(pcaResult$col),
                bty="o", pt.bg = bg, ncol = ncol)}

  if (labels == T)
    text3D(x = pcaResult$objects$scores[ ,axes[1]], y = pcaResult$objects$scores[ ,axes[2]], z = pcaResult$objects$scores[ ,axes[3]],
           labels  = pcaResult$objects$ID,
           add = TRUE, cex = 0.8)

}
