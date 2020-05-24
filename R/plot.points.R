#' The default scatterplot function
#' @export
plot.points <- function(result, ...) {
  UseMethod("plot.points")
}


#' @rdname pca.calc
#' @method plot.points pcadata
#' @export
plot.points.pcadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL,
                         pch = 16, col = "black", pt.bg = "white",
                         labels = FALSE,
                         legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

  checkClass(result, "pcadata")

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (max(axes) > length(result$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(result$eigenValues), " axes.", sep = "" ), call. = F)

  if (is.null(xlab))
    xlab = paste("PC",axes[1], " (", round(result$axesVariance[axes[1]]*100, digits = 2) ,"%)", sep = "")

  if (is.null(ylab))
    ylab = paste("PC",axes[2], " (", round(result$axesVariance[axes[2]]*100, digits = 2) ,"%)", sep = "")


  # nastav pch a col spravne podla taxonu
  result$pch = as.numeric( setValuesForVector(result$objects$Taxon, pch))
  result$col = setValuesForVector(result$objects$Taxon, col)
  result$pt.bg = setValuesForVector(result$objects$Taxon, pt.bg)

  # main plot

  plot(x = result$objects$scores[ ,axes[1]], y = result$objects$scores[ ,axes[2]],
       xlab = xlab, ylab = ylab, pch = result$pch, col = result$col, bg = result$pt.bg, ... )





  # legend
  if (legend) {
    #legendTable = cbind(as.character(result$objects$Taxon), result$pch, result$col, result$pt.bg)
    #legendTable = unique(legendTable)

    #plotLegend(legend.pos, legend = legendTable[,1],  pch = as.numeric(legendTable[,2]), col = legendTable[,3], pt.bg = legendTable[,4], ncol)
    plot.addLegend(result, x = legend.pos, pch = pch, col = col, pt.bg = pt.bg, ncol = ncol)
  }

  # labels
  if (labels) plot.addLabels.points(result, axes = axes, cex = 0.7, pos = 4, offset = 0.5)
    #plot2DLabels(result, axes)

}


#' @rdname cda.calc
#' @method plot.points cdadata
#' @export
plot.points.cdadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL,
                          pch = 16, col = "black", pt.bg = "white",
                          breaks = 1, ylim = NULL,
                          labels = FALSE,
                          legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

  checkClass(result, "cdadata")


  if (result$rank == 1) {
    # HISTOGRAM

    if (!(all(axes == c(1,2)) ||  (length(axes) == 1  && axes == 1))) warning("The object has only one axis, which will be plotted", call. = F)

    taxlev = levels(result$objects$Taxon)

    # breaks
    # musim to vyhodit z data.frame
    result$objects$scores = as.numeric(result$objects$scores[,])
    xhist = hist(result$objects$scores, plot = F)

    hist_breaks = seq(from = min(xhist$breaks), to = max(xhist$breaks), by = breaks )

    # nastav pch, col a pt.bg spravne podla taxonu
    result$col = setValuesForVector(result$objects$Taxon, "black") # farba prazdneho znaku, samotna farba bude v pt.bg
    if (col == "black" && pt.bg != "white") {
      col = pt.bg
    }
    result$pt.bg = setValuesForVector(result$objects$Taxon, col)

    ##########  REGION   Tuto to rob v cykle, lebo nevies kolko bude skupin

    # struktura pre skladovanie hystogramov
    histograms = list(list(list(),list(),list(),list(),list(),list()))

    for (i in 1:length(taxlev)) {
      histograms[[i]] = hist(result$objects$scores[result$objects$Taxon == taxlev[i]], plot = F, breaks = hist_breaks )
      histograms[[i]]$pt.bg = result$pt.bg[result$objects$Taxon == taxlev[i]][1]
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

    axis(1, at = hist_breaks, labels = hist_breaks, tcl = -0.5)
    axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10), tcl=-0.5)

    # legend
    if (legend) plot.addLegend(result, x = legend.pos, pch = 22, col = "black", pt.bg = col, ncol = ncol)

	# labels neplotujem
	if (labels)  warning("Labels = TRUE is not supported for histograms.")




  } else if (result$rank > 1)  {
    # SCATTERPLOT

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
    if (max(axes) > length(result$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(result$eigenValues), " axes.", sep = "" ), call. = F)

    if (is.null(xlab)) xlab = paste("Canonical axis ",axes[1], " (", round(result$axesVariance[axes[1]]*100, digits = 2) ,"%)", sep = "")
    if (is.null(ylab)) ylab = paste("Canonical axis ",axes[2], " (", round(result$axesVariance[axes[2]]*100, digits = 2) ,"%)", sep = "")

    # nastav pch a col spravne podla taxonu
    result$pch = as.numeric( setValuesForVector(result$objects$Taxon, pch))
    result$col = setValuesForVector(result$objects$Taxon, col)
    result$pt.bg = setValuesForVector(result$objects$Taxon, pt.bg)

    # main plot
    plot(x = result$objects$scores[ ,axes[1]], y = result$objects$scores[ ,axes[2]],
         xlab = xlab, ylab = ylab, pch = result$pch, col = result$col, bg = result$pt.bg, ... )


    # legend
    if (legend) plot.addLegend(result, x = legend.pos, pch = pch, col = col, pt.bg = pt.bg, ncol = ncol)


    # labels
    if (labels) plot.addLabels.points(result, axes = axes, cex = 0.7, pos = 4, offset = 0.5)

    }
}








