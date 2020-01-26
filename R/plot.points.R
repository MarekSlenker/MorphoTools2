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


  if (legend == TRUE) plotLegend(pcaResult, legend.pos, bg, ncol)

  if (labels == TRUE) plot2DLabels(pcaResult, axes)

}


#' @rdname cda.calc
#' @method plot.points cdadata
#' @export
plot.points.cdadata <- function(cdaResult, axes = c(1,2), xlab = NULL, ylab = NULL,
                                pch = 16, col = "black", breaks = NULL, ylim = NULL, bg = "white", labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

  checkClass(cdaResult, "cdadata")

  # nastav pch a col spravne podla taxonu
  cdaResult$pch = as.numeric( setValuesForVector(cdaResult$objects$Taxon, pch))
  cdaResult$col = setValuesForVector(cdaResult$objects$Taxon, col)

  if (cdaResult$rank == 1) {
    # HISTOGRAM

    taxlev = levels(cdaResult$objects$Taxon)

    if (is.null(breaks)) {xhist = hist(cdaResult$objects$scores, plot = F)
                          breaks = xhist$breaks }

    hist1 = hist(cdaResult$objects$scores[cdaResult$objects$Taxon == taxlev[1]], plot = F, breaks = breaks )
    hist1$col = cdaResult$col[cdaResult$objects$Taxon == taxlev[1]][1]
    hist2 = hist(cdaResult$objects$scores[cdaResult$objects$Taxon == taxlev[2]], plot = F, breaks = breaks )
    hist2$col = cdaResult$col[cdaResult$objects$Taxon == taxlev[2]][1]

    if (is.null(ylim)) {
      ymax = max( c(hist1$counts, hist2$counts))
      upperLim = ymax  %/% 10; if ((ymax %% 10) > 0) upperLim = upperLim + 1; upperLim = upperLim * 10
      ylim = c(0, upperLim)}

    plot(hist1, main="", xlab = "canonical score", ylab = "count", col = hist1$col, ylim = ylim, axes = F, ...)
    plot(hist2, col = hist2$col, axes = F, add = T, ...)

    axis(1, at = breaks, labels = breaks, tcl = -0.5)
    axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10), tcl=-0.5)

    # nastavujem defaultne Labels na False, aby to niekoho nenapadlo :)
    labels = FALSE


  } else if (cdaResult$rank > 1)  {
    # SCATTERPLOT

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
    if (max(axes) > length(cdaResult$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(cdaResult$eigenValues), " axes.", sep = "" ), call. = F)

    if (is.null(xlab))
      xlab = paste("Canonical axis ",axes[1], " (", round(cdaResult$axesVariance[axes[1]], digits = 2) ,"%)", sep = "")
    if (is.null(ylab))
      ylab = paste("Canonical axis ",axes[2], " (", round(cdaResult$axesVariance[axes[2]], digits = 2) ,"%)", sep = "")

    # main plot
    plot(x = cdaResult$objects$scores[ ,axes[1]], y = cdaResult$objects$scores[ ,axes[2]],
         xlab = xlab, ylab = ylab, pch = cdaResult$pch, col = cdaResult$col, bg = bg, ... )
  }


  if (legend == TRUE) plotLegend(cdaResult, legend.pos, bg, ncol)

  if (labels == TRUE) plot2DLabels(cdaResult, axes)


}








