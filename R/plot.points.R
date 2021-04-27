#' The default scatterplot function
#' @export
plotPoints <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL,
                        pch = 16, col = "black", pt.bg = "white",
                        breaks = 1, ylim = NULL, xlim = NULL,
                        labels = FALSE,
                        legend = FALSE, legend.pos = "topright", ncol = 1, ...) {
  UseMethod("plotPoints")
}


#' @rdname pca.calc
#' @method plotPoints pcadata
#' @export
plotPoints.pcadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL,
                         pch = 16, col = "black", pt.bg = "white",
                         breaks = 1, ylim = NULL, xlim = NULL,
                         labels = FALSE,
                         legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specify 2 axes (e.g., axes = c(1,2))", call. = FALSE)
  if (max(axes) > length(result$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(result$eigenValues), " axes.", sep = "" ), call. = FALSE)

  if (is.null(xlab))
    xlab = paste(names(result$eigenValues)[axes[1]], " (", round(result$eigenvaluesAsPercent[axes[1]]*100, digits = 2) ,"%)", sep = "")

  if (is.null(ylab))
    ylab = paste(names(result$eigenValues)[axes[2]], " (", round(result$eigenvaluesAsPercent[axes[2]]*100, digits = 2) ,"%)", sep = "")


  # nastav pch a col spravne podla taxonu
  result$pch = as.numeric( .setValuesForVector(result$objects$Taxon, pch))
  result$col = .setValuesForVector(result$objects$Taxon, col)
  result$pt.bg = .setValuesForVector(result$objects$Taxon, pt.bg)

  # main plot

  graphics::plot(x = result$objects$scores[ ,axes[1]], y = result$objects$scores[ ,axes[2]],
       xlab = xlab, ylab = ylab, pch = result$pch, col = result$col, bg = result$pt.bg, xlim = xlim, ylim = ylim, ... )





  # legend
  if (legend) {
    #legendTable = cbind(as.character(result$objects$Taxon), result$pch, result$col, result$pt.bg)
    #legendTable = unique(legendTable)

    #plotLegend(legend.pos, legend = legendTable[,1],  pch = as.numeric(legendTable[,2]), col = legendTable[,3], pt.bg = legendTable[,4], ncol)
    plotAddLegend(result, x = legend.pos, pch = pch, col = col, pt.bg = pt.bg, ncol = ncol)
  }

  # labels
  if (labels) plotAddLabels.points(result, axes = axes, cex = 0.7, pos = 4, offset = 0.5)
    #plot2DLabels(result, axes)

}


#' @rdname pcoa.calc
#' @method plotPoints pcoadata
#' @export
plotPoints.pcoadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL,
                               pch = 16, col = "black", pt.bg = "white",
                               breaks = 1, ylim = NULL, xlim = NULL,
                               labels = FALSE,
                               legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specify 2 axes (e.g., axes = c(1,2))", call. = FALSE)
  if (max(axes) > length(result$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(result$eigenValues), " axes.", sep = "" ), call. = FALSE)

  if (is.null(xlab))
    xlab = paste(names(result$eigenValues)[axes[1]], " (", round(result$eigenvaluesAsPercent[axes[1]]*100, digits = 2) ,"%)", sep = "")

  if (is.null(ylab))
    ylab = paste(names(result$eigenValues)[axes[2]], " (", round(result$eigenvaluesAsPercent[axes[2]]*100, digits = 2) ,"%)", sep = "")


  # nastav pch a col spravne podla taxonu
  result$pch = as.numeric( .setValuesForVector(result$objects$Taxon, pch))
  result$col = .setValuesForVector(result$objects$Taxon, col)
  result$pt.bg = .setValuesForVector(result$objects$Taxon, pt.bg)

  # main plot

  graphics::plot(x = result$objects$scores[ ,axes[1]], y = result$objects$scores[ ,axes[2]],
                 xlab = xlab, ylab = ylab, pch = result$pch, col = result$col, bg = result$pt.bg, xlim = xlim, ylim = ylim, ... )





  # legend
  if (legend) {
    plotAddLegend(result, x = legend.pos, pch = pch, col = col, pt.bg = pt.bg, ncol = ncol)
  }

  # labels
  if (labels) plotAddLabels.points(result, axes = axes, cex = 0.7, pos = 4, offset = 0.5)

}





#' @rdname cda.calc
#' @method plotPoints cdadata
#' @export
plotPoints.cdadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL,
                          pch = 16, col = "black", pt.bg = "white",
                          breaks = 1, ylim = NULL, xlim = NULL,
                          labels = FALSE,
                          legend = FALSE, legend.pos = "topright", ncol = 1, ...) {


  if (result$rank == 1) {
    # HISTOGRAM

    if (length(axes) > 1){
             if (!(all(axes == c(1,2)))) warning("The object has only one axis, which will be plotted", call. = FALSE)
    }

    if (length(axes) == 1){
            if (axes != 1) warning("The object has only one axis, which will be plotted", call. = FALSE)
    }

    taxlev = levels(result$objects$Taxon)

    # breaks
    # musim to vyhodit z data.frame
    result$objects$scores = as.numeric(result$objects$scores[,])
    xhist = graphics::hist(result$objects$scores, plot = FALSE)

    hist_breaks = seq(from = min(xhist$breaks), to = max(xhist$breaks), by = breaks )

    # nastav pch, col a pt.bg spravne podla taxonu
    result$col = .setValuesForVector(result$objects$Taxon, "black") # farba prazdneho znaku, samotna farba bude v pt.bg

    if (length(col) == 1) {
      if (col == "black") {

        if ((length(pt.bg) == 1)) {
          if (pt.bg != "white") {
            col = pt.bg
          }
        }

        if ((length(pt.bg) > 1)) {
          col = pt.bg
        }
      }
    }

    result$pt.bg = .setValuesForVector(result$objects$Taxon, col)

    ##########  REGION   Tuto to rob v cykle, lebo nevies kolko bude skupin

    # struktura pre skladovanie hystogramov
    histograms = list(list(list(),list(),list(),list(),list(),list()))

    for (i in 1:length(taxlev)) {
      histograms[[i]] = graphics::hist(result$objects$scores[result$objects$Taxon == taxlev[i]], plot = FALSE, breaks = hist_breaks )
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

    if (is.null(xlim)) {
      xlim = c(min(hist_breaks), max(hist_breaks))
    }

    #   plotni v cykle
    graphics::plot(histograms[[1]], main="", xlab = "canonical score", ylab = "count", col = histograms[[1]]$pt.bg, ylim = ylim, xlim = xlim, axes = FALSE, ...)
    for (i in 2:length(taxlev)) {
      graphics::plot(histograms[[i]], col = histograms[[i]]$pt.bg, axes = F, add = TRUE)
    }

    ########### ENDREGION

    graphics::axis(1, at = hist_breaks, labels = hist_breaks, tcl = -0.5)
    graphics::axis(2, at = seq(ylim[1], ylim[2], 10), labels = seq(ylim[1], ylim[2], 10), tcl=-0.5)

    # legend
    if (legend) plotAddLegend(result, x = legend.pos, pch = 22, col = "black", pt.bg = col, ncol = ncol)

	# labels neplotujem
	if (labels)  warning("Labels = TRUE is not supported for histograms.")




  }
  else if (result$rank > 1)  {
    # SCATTERPLOT

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("you have to specify 2 axes (e.g., axes = c(1,2))", call. = FALSE)
    if (max(axes) > length(result$eigenValues)) stop(paste("specified axes are out of bounds. Object has only ", length(result$eigenValues), " axes.", sep = "" ), call. = FALSE)

    if (is.null(xlab)) xlab = paste(names(result$eigenValues)[axes[1]], " (", round(result$eigenvaluesAsPercent[axes[1]]*100, digits = 2) ,"%)", sep = "")
    if (is.null(ylab)) ylab = paste(names(result$eigenValues)[axes[2]], " (", round(result$eigenvaluesAsPercent[axes[2]]*100, digits = 2) ,"%)", sep = "")

    # nastav pch a col spravne podla taxonu
    result$pch = as.numeric( .setValuesForVector(result$objects$Taxon, pch))
    result$col = .setValuesForVector(result$objects$Taxon, col)
    result$pt.bg = .setValuesForVector(result$objects$Taxon, pt.bg)

    # main plot
    graphics::plot(x = result$objects$scores[ ,axes[1]], y = result$objects$scores[ ,axes[2]],
         xlab = xlab, ylab = ylab, pch = result$pch, col = result$col, bg = result$pt.bg, xlim = xlim, ylim = ylim, ... )


    # legend
    if (legend) plotAddLegend(result, x = legend.pos, pch = pch, col = col, pt.bg = pt.bg, ncol = ncol)


    # labels
    if (labels) plotAddLabels.points(result, axes = axes, cex = 0.7, pos = 4, offset = 0.5)

    }
}








