#' The default scatterplot function
#' @export
plot.points <- function(object, ...) {
  UseMethod("plot.points")
}


#' @rdname pca.calc
#' @method plot.points pcadata
#' @export
plot.points.pcadata <- function(pcaResult, axes = c(1,2), xlab = NULL, ylab = NULL,
                         pch = 16, col = "black", pt.bg = "white",
                         labels = FALSE,
                         legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

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
  pcaResult$pt.bg = setValuesForVector(pcaResult$objects$Taxon, pt.bg)

  # main plot

  plot(x = pcaResult$objects$scores[ ,axes[1]], y = pcaResult$objects$scores[ ,axes[2]],
       xlab = xlab, ylab = ylab, pch = pcaResult$pch, col = pcaResult$col, bg = pcaResult$pt.bg, ... )





  # legend
  if (legend == TRUE) {
    #legendTable = cbind(as.character(pcaResult$objects$Taxon), pcaResult$pch, pcaResult$col, pcaResult$pt.bg)
    #legendTable = unique(legendTable)

    #plotLegend(legend.pos, legend = legendTable[,1],  pch = as.numeric(legendTable[,2]), col = legendTable[,3], pt.bg = legendTable[,4], ncol)
    plot.addLegend(pcaResult, x = legend.pos, pch = pch, col = col, pt.bg = pt.bg, ncol = ncol)
  }

  # labels
  if (labels == TRUE) plot.addLabels.points(pcaResult, axes = axes, cex = 0.7, pos = 4, offset = 0.5)
    #plot2DLabels(pcaResult, axes)

}


#' @rdname cda.calc
#' @method plot.points cdadata
#' @export
plot.points.cdadata <- function(cdaResult, axes = c(1,2), xlab = NULL, ylab = NULL,
                          pch = 16, col = "black", pt.bg = "white",
                          breaks = NULL, ylim = NULL,
                          labels = FALSE,
                          legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

  checkClass(cdaResult, "cdadata")


  if (cdaResult$rank == 1) {
    # HISTOGRAM

    taxlev = levels(cdaResult$objects$Taxon)

    # musim to vyhodit z data.frame
    cdaResult$objects$scores = as.numeric(cdaResult$objects$scores[,])

    xhist = hist(cdaResult$objects$scores, plot = F)

    if (is.null(breaks)) breaks = xhist$breaks

    # nastav pch, col a pt.bg spravne podla taxonu
    cdaResult$col = setValuesForVector(cdaResult$objects$Taxon, "black") # farba prazdneho znaku, samotna farba bude v pt.bg
    if (col == "black" && pt.bg != "white") {
      col = pt.bg
    }
    cdaResult$pt.bg = setValuesForVector(cdaResult$objects$Taxon, col)

    ##########  REGION   Tuto to rob v cykle, lebo nevies kolko bude skupin

    # struktura pre skladovanie hystogramov
    histograms = list(list(list(),list(),list(),list(),list(),list()))

    for (i in 1:length(taxlev)) {
      histograms[[i]] = hist(cdaResult$objects$scores[cdaResult$objects$Taxon == taxlev[i]], plot = F, breaks = breaks )
      histograms[[i]]$pt.bg = cdaResult$pt.bg[cdaResult$objects$Taxon == taxlev[i]][1]
    }

    #   MAX porovnanaj v cykle, na konci cyklu budes mat max zo vsetkych - na nastavien ylim
    ymax = 0
    if (is.null(ylim)) {
      for (i in 1:length(taxlev)) {
        ymax = max( c(ymax, histograms[[i]]$counts))
      }
      # hrajkanie sa s delenim a zvyskom po delenie, aby som nasiel nablizsie cislo delitelne 10
      upperLim = ymax  %/% 10; if ((ymax %% 10) > 0) upperLim = upperLim + 1; upperLim = upperLim * 10
      ylim = c(0, upperLim)
    }

    #   plotni v cykle
    plot(histograms[[1]], main="", xlab = "canonical score", ylab = "count", col = histograms[[1]]$pt.bg, ylim = ylim, axes = F, ...)
    for (i in 2:length(taxlev)) {
      plot(histograms[[i]], col = histograms[[i]]$pt.bg, axes = F, add = T)
    }

    ########### ENDREGION

    axis(1, at = breaks, labels = breaks, tcl = -0.5)
    axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10), tcl=-0.5)

    # legend
    if (legend == TRUE) plot.addLegend(cdaResult, x = legend.pos, pch = 22, col = "black", pt.bg = col, ncol = ncol)


    # labels neplotujem


  } else if (cdaResult$rank > 1)  {
    # SCATTERPLOT

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
    if (max(axes) > length(cdaResult$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(cdaResult$eigenValues), " axes.", sep = "" ), call. = F)

    if (is.null(xlab)) xlab = paste("Canonical axis ",axes[1], " (", round(cdaResult$axesVariance[axes[1]]*100, digits = 2) ,"%)", sep = "")
    if (is.null(ylab)) ylab = paste("Canonical axis ",axes[2], " (", round(cdaResult$axesVariance[axes[2]]*100, digits = 2) ,"%)", sep = "")

    # nastav pch a col spravne podla taxonu
    cdaResult$pch = as.numeric( setValuesForVector(cdaResult$objects$Taxon, pch))
    cdaResult$col = setValuesForVector(cdaResult$objects$Taxon, col)
    cdaResult$pt.bg = setValuesForVector(cdaResult$objects$Taxon, pt.bg)

    # main plot
    plot(x = cdaResult$objects$scores[ ,axes[1]], y = cdaResult$objects$scores[ ,axes[2]],
         xlab = xlab, ylab = ylab, pch = cdaResult$pch, col = cdaResult$col, bg = cdaResult$pt.bg, ... )


    # legend
    if (legend == TRUE) plot.addLegend(cdaResult, x = legend.pos, pch = pch, col = col, pt.bg = pt.bg, ncol = ncol)


    # labels
    if (labels == TRUE) plot.addLabels.points(cdaResult, axes = axes, cex = 0.7, pos = 4, offset = 0.5)

    }
}








