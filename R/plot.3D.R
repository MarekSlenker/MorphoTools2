# generic function


#' The default scatterplot 3D function
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
#' @method plot.3D pcadata
#' @export
plot.3D.pcadata <- function(pcaResult, axes = c(1,2,3), xlab = NULL, ylab = NULL, zlab = NULL,
                            pch = 16, col = "black", pt.bg = "white",
                            phi = 10, theta = 2,
                            ticktype = "detailed", bty = "u", type = "n",
                            labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...) {
  checkClass(pcaResult, "pcadata")

  # skontroluj ci axes = 3; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 3) stop("you have to specifi 3 axes (e.g., axes = c(1,2,3))", call. = F)
  if (max(axes) > length(pcaResult$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(pcaResult$eigenValues), " axes.", sep = "" ), call. = F)

  if (is.null(xlab)) xlab = paste("PC",axes[1], " (", round(pcaResult$axesVariance[axes[1]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(ylab)) ylab = paste("PC",axes[2], " (", round(pcaResult$axesVariance[axes[2]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(zlab)) zlab = paste("PC",axes[3], " (", round(pcaResult$axesVariance[axes[3]]*100, digits = 2) ,"%)", sep = "")


  # nastav pch a col spravne podla taxonu
  pcaResult$pch = as.numeric( setValuesForVector(pcaResult$objects$Taxon, pch))
  pcaResult$col = setValuesForVector(pcaResult$objects$Taxon, col)
  pcaResult$pt.bg = setValuesForVector(pcaResult$objects$Taxon, pt.bg)


  # main plot
  scatter3D(x = pcaResult$objects$scores[ ,axes[1]], y = pcaResult$objects$scores[ ,axes[2]], z = pcaResult$objects$scores[ ,axes[3]],
            colvar = NULL, colkey = T,
             col = pcaResult$col, pch = pcaResult$pch, bg.col = pcaResult$pt.bg,
             phi = phi, theta = theta,
             ticktype = ticktype, bty = bty, type = type,
            xlab = xlab, ylab = ylab, zlab = zlab, ...)



  if (legend == TRUE) plotLegend(pcaResult, legend.pos, pch = unique(pcaResult$pch), col = unique(pcaResult$col), pt.bg = unique(pcaResult$pt.bg), ncol)

  # labels
  {
  if (labels == T)
    text3D(x = pcaResult$objects$scores[ ,axes[1]], y = pcaResult$objects$scores[ ,axes[2]], z = pcaResult$objects$scores[ ,axes[3]],
           labels  = pcaResult$objects$ID,
           add = TRUE, cex = 0.8)
  }
}
