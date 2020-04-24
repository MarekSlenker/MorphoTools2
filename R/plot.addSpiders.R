

#' Add spiders to a plot
#' @export
plot.addSpiders <- function(result, ...) {
  UseMethod("plot.addSpiders")
}


#' @rdname plot.addSpiders
#' @method plot.addSpiders pcadata
#' @export
plot.addSpiders.pcadata <- function(pcaResult, axes = c(1,2), col = "black", lty = 1, lwd = 1, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (max(axes) > pcaResult$rank) stop(paste("specified axes are out of bounds. Object has only ", pcaResult$rank, " axes.", sep = "" ), call. = F)

  plot_spiders_internal(pcaResult, axes = axes, col = col, lty = lty, lwd = lwd, ...)
}

#' @rdname plot.addSpiders
#' @method plot.addSpiders cdadata
#' @export
plot.addSpiders.cdadata <- function(cdaResult, axes = c(1,2), col = "black", lty = 1, lwd = 1, ...) {
  # hist
  if (cdaResult$rank == 1) {

    stop("The method plot.addSpiders() is not applicable to histogram.", call. = F)
  }

  if (cdaResult$rank > 1)  {

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
    if (max(axes) > cdaResult$rank) stop(paste("specified axes are out of bounds. Object has only ", cdaResult$rank, " axes.", sep = "" ), call. = F)

    plot_spiders_internal(cdaResult, axes = axes, col = col, lty = lty, lwd = lwd, ...)
  }
}


plot_spiders_internal <- function(result, axes, col, lty, lwd, ...) {

  result$col = setValuesForVector(result$objects$Taxon, col)

  scores = data.frame("Taxon" = result$objects$Taxon,
                     "score1" = result$objects$scores[, axes[1]],
                     "score2" = result$objects$scores[, axes[2]],
                     "col" = result$col)

  centroids = data.frame(result$groupMeans$Taxon, result$groupMeans[axes[1]+1], result$groupMeans[axes[2]+1])
  colnames(centroids) = c("Taxon", "cen1", "cen2")


  taxData = merge(scores, centroids, by="Taxon")

  segments(x0=taxData$cen1, y0=taxData$cen2, x1=taxData$score1 , y1=taxData$score2, col = as.character(taxData$col), lwd = lwd, lty = lty, ...)



}






