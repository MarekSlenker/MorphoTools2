#' The default biplot function
#' @export
plotBiplot <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL, pch = 16, col = "black", pt.bg = "white", breaks = 1,
                       xlim = NULL, ylim = NULL,
                       labels = FALSE, arrowLabels = TRUE,
                       angle = 15, length = 0.1, arrowCol = "red",
                       legend = FALSE, legend.pos = "topright", ncol = 1, ...) {
  UseMethod("plotBiplot")
}


#' @rdname plotBiplot
#' @method plotBiplot pcadata
#' @export
plotBiplot.pcadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL, pch = 16, col = "black", pt.bg = "white", breaks = 1,
                               xlim = NULL, ylim = NULL,
                               labels = FALSE, arrowLabels = TRUE,
                               angle = 15, length = 0.1, arrowCol = "red",
                               legend = FALSE, legend.pos = "topright", ncol = 1, ...) {



  plotPoints(result, axes = axes, xlab = xlab, ylab = ylab, pch = pch, col = col, pt.bg = pt.bg, breaks = breaks, ylim = ylim, xlim = xlim,
                                 labels = labels, legend = legend, legend.pos = legend.pos, ncol = ncol, ...)


  # rescale variable coordinates
  r = min(
    max(result$objects$scores[ ,axes[1]])/max(result$eigenvectors[ ,axes[1]]),
    abs(min(result$objects$scores[ ,axes[1]]))/abs(min(result$eigenvectors[ ,axes[1]])),
    max(result$objects$scores[ ,axes[2]])/max(result$eigenvectors[ ,axes[2]]),
    abs(min(result$objects$scores[ ,axes[2]]))/abs(min(result$eigenvectors[ ,axes[2]]))
  )
  # r = min(
  #   (min( max(result$objects$scores[ ,axes[1]]), abs(min(result$objects$scores[ ,axes[1]])) ) /
  #      min( max(result$eigenvectors[ ,axes[1]]), abs(min(result$eigenvectors[ ,axes[1]])) )),

  #   (min( max(result$objects$scores[ ,axes[2]]), abs(min(result$objects$scores[ ,axes[2]])) ) /
  #      min( max(result$eigenvectors[ ,axes[2]]), abs(min(result$eigenvectors[ ,axes[2]])) ))
  # )
  result$eigenvectors[ ,axes[1]] = result$eigenvectors[ ,axes[1]]*r*0.8
  result$eigenvectors[ ,axes[2]] = result$eigenvectors[ ,axes[2]]*r*0.8

  graphics::abline(h = 0,v = 0,lty = 2,col = "grey")
  graphics::arrows(0, 0, result$eigenvectors[ ,axes[1]], result$eigenvectors[ ,axes[2]],
                   col = arrowCol, length = length, angle = angle, ...)



  # labels
  if (arrowLabels) plotAddLabels.characters(result, axes = axes, col = arrowCol)

}



#' @rdname plotBiplot
#' @method plotBiplot cdadata
#' @export
plotBiplot.cdadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL, pch = 16, col = "black", pt.bg = "white", breaks = 1,
                               xlim = NULL, ylim = NULL,
                               labels = FALSE, arrowLabels = TRUE,
                               angle = 15, length = 0.1, arrowCol = "red",
                               legend = FALSE, legend.pos = "topright", ncol = 1, ...) {



  plotPoints(result, axes = axes, xlab = xlab, ylab = ylab, pch = pch, col = col, pt.bg = pt.bg, breaks = breaks, ylim = ylim, xlim = xlim,
             labels = labels, legend = legend, legend.pos = legend.pos, ncol = ncol, ...)



  # rescale variable coordinates
  if (result$rank == 1) {
    # HISTOGRAMOVE
    r = min( max(result$objects$scores)/max(result$totalCanonicalStructure),
             abs(min(result$objects$scores))/abs(min(result$totalCanonicalStructure))    )
  } else {
    # rescale variable coordinates
    r = min(
      max(result$objects$scores[ ,axes[1]])/max(result$totalCanonicalStructure[ ,axes[1]]),
      abs(min(result$objects$scores[ ,axes[1]]))/abs(min(result$totalCanonicalStructure[ ,axes[1]])),
      max(result$objects$scores[ ,axes[2]])/max(result$totalCanonicalStructure[ ,axes[2]]),
      abs(min(result$objects$scores[ ,axes[2]]))/abs(min(result$totalCanonicalStructure[ ,axes[2]]))
    )

    # r = min(
    #    (min( max(result$objects$scores[ ,axes[1]]), abs(min(result$objects$scores[ ,axes[1]])) ) /
    #     min( max(result$totalCanonicalStructure[ ,axes[1]]), abs(min(result$totalCanonicalStructure[ ,axes[1]])) )),

    #   (min( max(result$objects$scores[ ,axes[2]]), abs(min(result$objects$scores[ ,axes[2]])) ) /
    #  min( max(result$totalCanonicalStructure[ ,axes[2]]), abs(min(result$totalCanonicalStructure[ ,axes[2]])) ))

    # )
  }
  result$totalCanonicalStructure = result$totalCanonicalStructure*r*0.8

  # plotCharacters(result)   --------------------TOTO MUSIS ODZNOVA

  if (result$rank == 1) {
    # Height of plot BEGIN
    taxlev = levels(result$objects$Taxon)
    scores = as.numeric(result$objects$scores[,])
    xhist = graphics::hist(scores, plot = FALSE)
    hist_breaks = seq(from = min(xhist$breaks), to = max(xhist$breaks), by = breaks )
    # struktura pre skladovanie hystogramov
    histograms = list(list(list(),list(),list(),list(),list(),list()))
    for (i in 1:length(taxlev)) {
      histograms[[i]] = graphics::hist(scores[result$objects$Taxon == taxlev[i]], plot = FALSE, breaks = hist_breaks )
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
      y = seq(upperLim*0.9, 1,length.out = length(result$totalCanonicalStructure[,1]))
    } else {
      y = seq(max(ylim)*0.9, 1,length.out = length(result$totalCanonicalStructure[,1]))
    }
    # Height of plot END



    graphics::abline(v = 0,lty = 2,col = "grey")
    graphics::arrows(x0 = 0, y0 = y, x1 = result$totalCanonicalStructure[,1], y1 = y, col = arrowCol, length = length, angle = angle, ...)

  } else {

    # SCATTER
    graphics::abline(h = 0,v = 0,lty = 2,col = "grey")
    graphics::arrows(0, 0, result$totalCanonicalStructure[,axes[1]], result$totalCanonicalStructure[,axes[2]],
                     col = arrowCol, length = length, angle = angle, ...)
  }


  # labels
  if (arrowLabels) plotAddLabels.characters(result, axes = axes, col = arrowCol, breaks = breaks)

}
