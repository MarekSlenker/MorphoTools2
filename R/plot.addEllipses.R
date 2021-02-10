

#' Add confidence ellipses to a plot
#' @export
plotAddEllipses <- function(result, axes = c(1,2), probability = 0.95, col = "black", type = "l", lty = 1, lwd = 1, ...) {
  UseMethod("plotAddEllipses")
}


#' @rdname plotAddEllipses
#' @method plotAddEllipses pcadata
#' @export
plotAddEllipses.pcadata <- function(result, axes = c(1,2), probability = 0.95, col = "black", type = "l", lty = 1, lwd = 1, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specify 2 axes (e.g., axes = c(1,2))", call. = FALSE)
  if (max(axes) > result$rank) stop(paste("specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

  plot_ellipses_internal(result, axes = axes, probability = probability, col = col, type = type, lty = lty, lwd = lwd, ...)
}

#' @rdname plotAddEllipses
#' @method plotAddEllipses cdadata
#' @export
plotAddEllipses.cdadata <- function(result, axes = c(1,2), probability = 0.95, col = "black", type = "l", lty = 1, lwd = 1, ...) {
  # hist
  if (result$rank == 1) {

    stop("The method plotAddEllipses() is not applicable to histogram.", call. = FALSE)
  }

  if (result$rank > 1)  {

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("you have to specify 2 axes (e.g., axes = c(1,2))", call. = FALSE)
    if (max(axes) > result$rank) stop(paste("specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

    plot_ellipses_internal(result, axes = axes, probability = probability, col = col, type = type, lty = lty, lwd = lwd, ...)
  }
}


plot_ellipses_internal <- function(result, axes, probability, col, type, lty, lwd, ...) {

  result$col = setValuesForVector(result$objects$Taxon, col)

  tt = sqrt(stats::qchisq(probability, 2)) # kvantil rozdelenia
  if (is.infinite(tt)) stop(paste("Probability = ", probability, " caused infinite size of ellipses.", sep = ""), call. = FALSE)

  for (taxon in levels(result$objects$Taxon)) {
    taxData = data.frame(result$objects$scores[which(taxon == result$objects$Taxon),  axes[1]],
                         result$objects$scores[which(taxon == result$objects$Taxon),  axes[2]],
                         result$col[which(taxon == result$objects$Taxon)]                         )
    covMat = stats::cov.wt(taxData[,-3])

    elip = ellipse::ellipse(covMat$cov, centre = covMat$center, t = tt, FUN = graphics::lines) # predikcna elipsa
    taxCol = unique(taxData[,3])

    graphics::lines(elip, col = as.character(taxCol), type = type, lty = lty, lwd = lwd, ...)
  }

}






