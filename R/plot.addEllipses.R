

#' Add confidence ellipses to a plot
#' @export
plot.addEllipses <- function(result, ...) {
  UseMethod("plot.addEllipses")
}


#' @rdname plot.addEllipses
#' @method plot.addEllipses pcadata
#' @export
plot.addEllipses.pcadata <- function(pcaResult, axes = c(1,2), probability = 0.95, col = "black", type = "l", lty = 1, lwd = 1, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (max(axes) > pcaResult$rank) stop(paste("specified axes are out of bounds. Object has only ", pcaResult$rank, " axes.", sep = "" ), call. = F)

  plot_ellipses_internal(pcaResult, axes = axes, probability = probability, col = col, type = type, lty = lty, lwd = lwd, ...)
}

#' @rdname plot.addEllipses
#' @method plot.addEllipses cdadata
#' @export
plot.addEllipses.cdadata <- function(cdaResult, axes = c(1,2), probability = 0.95, col = "black", type = "l", lty = 1, lwd = 1, ...) {
  # hist
  if (cdaResult$rank == 1) {

    stop("The method plot.addEllipses() is not applicable to histogram.", call. = F)
  }

  if (cdaResult$rank > 1)  {

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
    if (max(axes) > cdaResult$rank) stop(paste("specified axes are out of bounds. Object has only ", cdaResult$rank, " axes.", sep = "" ), call. = F)

    plot_ellipses_internal(cdaResult, axes = axes, probability = probability, col = col, type = type, lty = lty, lwd = lwd, ...)
  }
}


plot_ellipses_internal <- function(result, axes, probability, col, type, lty, lwd, ...) {

  result$col = setValuesForVector(result$objects$Taxon, col)

  T = sqrt(qchisq(probability, 2)) # kvantil rozdelenia
  if (is.infinite(T)) stop(paste("Probability = ", probability, " caused infinite size of ellipses.", sep = ""), call. = F)

  for (taxon in levels(result$objects$Taxon)) {
    taxData = data.frame(result$objects$scores[which(taxon == result$objects$Taxon),  axes[1]],
                         result$objects$scores[which(taxon == result$objects$Taxon),  axes[2]],
                         result$col[which(taxon == result$objects$Taxon)]                         )
    covMat = cov.wt(taxData[,-3])

    elip = ellipse(covMat$cov, centre = covMat$center, t = T, FUN = lines) # predikcna elipsa
    taxCol = unique(taxData[,3])

    lines(elip, col = as.character(taxCol), type = type, lty = lty, lwd = lwd, ...)
  }

}






