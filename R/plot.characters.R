#' Draws characters contribution as arrows
#' @export
plotCharacters <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL, main = NULL, xlim = NULL, ylim = NULL,
                            col = "red", length = 0.1, angle = 15, labels = TRUE, cex = 0.7, ...) {
  UseMethod("plotCharacters")
}


#' @rdname plotCharacters
#' @method plotCharacters pcadata
#' @export
plotCharacters.pcadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL, main = NULL, xlim = NULL, ylim = NULL,
                            col = "red", length = 0.1, angle = 15, labels = TRUE, cex = 0.7, ...) {
  .checkClass(result, "pcadata")

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("You have to specify 2 axes (e.g., axes = c(1,2)).", call. = FALSE)
  if (max(axes) > length(result$eigenvalues)) stop(paste("Specified axes are out of bounds. Object has only ", length(result$eigenvalues), " axes.", sep = "" ), call. = FALSE)

  if (is.null(xlab)) xlab = paste(names(result$eigenvalues)[axes[1]])
  if (is.null(ylab)) ylab = paste(names(result$eigenvalues)[axes[2]])
  if (is.null(main)) main = "Eigenvectors"

  if (is.null(xlim)) xlim = c(max(abs(result$eigenvectors[ ,axes[1]]))*-1, max(abs(result$eigenvectors[ ,axes[1]])))* 1.05 # + 5%
  if (is.null(ylim)) ylim = c(max(abs(result$eigenvectors[ ,axes[2]]))*-1, max(abs(result$eigenvectors[ ,axes[2]])))* 1.05 # + 5%


  # main plot

  graphics::plot(x = result$eigenvectors[ ,axes[1]], y = result$eigenvectors[ ,axes[2]],
         type = "n", xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, main = main)
  graphics::abline(h = 0,v = 0,lty = 2,col = "grey")
  graphics::arrows(0, 0, result$eigenvectors[ ,axes[1]], result$eigenvectors[ ,axes[2]],
         col = col, length = length, angle = angle, ...)

  if (labels) {
    labs = row.names(result$eigenvectors)
    for (ch in 1:nrow(result$eigenvectors)) {
      # hore
      if (result$eigenvectors[ ,axes[2]][ch] > 0) graphics::text(x = result$eigenvectors[ ,axes[1]][ch], y = result$eigenvectors[ ,axes[2]][ch],
               labels = labs[ch], cex = cex, pos = 3, offset = 0.5)
      # dole
      else graphics::text(x = result$eigenvectors[ ,axes[1]][ch], y = result$eigenvectors[ ,axes[2]][ch],
             labels = labs[ch], cex = cex, pos = 1, offset = 0.5)
      }
  }

}



#' @rdname plotCharacters
#' @method plotCharacters cdadata
#' @export
plotCharacters.cdadata <- function(result, axes = c(1,2), xlab = NULL, ylab = NULL, main = NULL, xlim = NULL, ylim = NULL,
                                    col = "red", length = 0.1, angle = 15, labels = TRUE, cex = 0.7, ...) {

  .checkClass(result, "cdadata")

    if (is.null(main)) main = "Total canonical structure coefficients"

  if (result$rank == 1) {
    # HISTOGRAMOVE

    # odlisne od default, alebo nie 1
    if (!(all(axes == c(1,2)) ||  (length(axes) == 1  && axes == 1))) warning("The object has only one axis, which will be plotted.", call. = FALSE)


    if (is.null(xlab)) xlab = "Contribution of characters"
    if (is.null(ylab)) ylab = "Characters"
    if (is.null(xlim)) xlim = c(max(abs(result$totalCanonicalStructure[,1]))*-1, max(abs(result$totalCanonicalStructure[,1])))* 1.05 # + 5%

    # main plot

    y = seq(length(result$totalCanonicalStructure[,1]), 1, -1)

    graphics::plot(x = result$totalCanonicalStructure[,1], y = y, xlab = xlab, ylab = ylab, xlim = xlim,
         ylim = c(0,length(result$totalCanonicalStructure[,1])+1),type = "n", yaxt = "n", main = main)

    graphics::abline(v = 0,lty = 2,col = "grey")
    graphics::arrows(x0 = 0, y0 = y, x1 = result$totalCanonicalStructure[,1], y1 = y, col = col, length = length, angle = angle, ...)

    if (labels) {
      labs = row.names(result$totalCanonicalStructure)
      for (ch in 1:nrow(result$totalCanonicalStructure)) {
        # hore
        if (result$totalCanonicalStructure[ch] > 0) graphics::text(x = result$totalCanonicalStructure[ch], y = y[ch],
                                                            labels = labs[ch], cex = cex, pos = 4, offset = 0.5)
        # dole
        else graphics::text(x = result$totalCanonicalStructure[ch], y = y[ch],
                  labels = labs[ch], cex = cex, pos = 2, offset = 0.5)
      }
    }


  } else if (result$rank > 1)  {

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("You have to specify 2 axes (e.g., axes = c(1,2)).", call. = FALSE)
    if (max(axes) > ncol(result$totalCanonicalStructure)) stop(paste("Specified axes are out of bounds. Object has only ", ncol(result$totalCanonicalStructure), " axes.", sep = "" ), call. = FALSE)


    if (is.null(xlab)) xlab = paste(names(result$eigenvalues)[axes[1]])
    if (is.null(ylab)) ylab = paste(names(result$eigenvalues)[axes[2]])

    if (is.null(xlim)) xlim = c(max(abs(result$totalCanonicalStructure[ ,axes[1]]))*-1, max(abs(result$totalCanonicalStructure[ ,axes[1]])))* 1.05 # + 5%
    if (is.null(ylim)) ylim = c(max(abs(result$totalCanonicalStructure[ ,axes[2]]))*-1, max(abs(result$totalCanonicalStructure[ ,axes[2]])))* 1.05 # + 5%


    # main plot

    graphics::plot(x = result$totalCanonicalStructure[,axes[1]], y = result$totalCanonicalStructure[,axes[2]],
         xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, type = "n", main = main)

    graphics::abline(h = 0,v = 0,lty = 2,col = "grey")
    graphics::arrows(0, 0, result$totalCanonicalStructure[,axes[1]], result$totalCanonicalStructure[,axes[2]],
           col = col, length = length, angle = angle, ...)

    if (labels) {
      labs = row.names(result$totalCanonicalStructure)
      for (ch in 1:nrow(result$totalCanonicalStructure)) {
        # hore
        if (result$totalCanonicalStructure[ ,axes[2]][ch] > 0)
          graphics::text(x = result$totalCanonicalStructure[ ,axes[1]][ch], y = result$totalCanonicalStructure[ ,axes[2]][ch],
                                                            labels = labs[ch], cex = cex, pos = 3, offset = 0.5)
        # dole
        else graphics::text(x = result$totalCanonicalStructure[ ,axes[1]][ch], y = result$totalCanonicalStructure[ ,axes[2]][ch],
                  labels = labs[ch], cex = cex, pos = 1, offset = 0.5)
      }
    }

  }
}


