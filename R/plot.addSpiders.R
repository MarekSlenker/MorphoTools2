

#' Add spiders to a plot
#' @export
plotAddSpiders <- function(result, axes = c(1,2), col = "black", lty = 1, lwd = 1, ...) {
  UseMethod("plotAddSpiders")
}


#' @rdname plotAddSpiders
#' @method plotAddSpiders pcadata
#' @export
plotAddSpiders.pcadata <- function(result, axes = c(1,2), col = "black", lty = 1, lwd = 1, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("You have to specify 2 axes (e.g., axes = c(1,2)).", call. = FALSE)
  if (max(axes) > result$rank) stop(paste("Specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

  .plot_spiders_internal(result, axes = axes, col = col, lty = lty, lwd = lwd, ...)
}



#' @rdname plotAddSpiders
#' @method plotAddSpiders pcoadata
#' @export
plotAddSpiders.pcoadata <- function(result, axes = c(1,2), col = "black", lty = 1, lwd = 1, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("You have to specify 2 axes (e.g., axes = c(1,2)).", call. = FALSE)
  if (max(axes) > result$rank) stop(paste("Specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

  .plot_spiders_internal(result, axes = axes, col = col, lty = lty, lwd = lwd, ...)
}


#' @rdname plotAddSpiders
#' @method plotAddSpiders nmdsdata
#' @export
plotAddSpiders.nmdsdata <- function(result, axes = c(1,2), col = "black", lty = 1, lwd = 1, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("You have to specify 2 axes (e.g., axes = c(1,2)).", call. = FALSE)
  if (max(axes) > result$rank) stop(paste("Specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

  .plot_spiders_internal(result, axes = axes, col = col, lty = lty, lwd = lwd, ...)
}




#' @rdname plotAddSpiders
#' @method plotAddSpiders cdadata
#' @export
plotAddSpiders.cdadata <- function(result, axes = c(1,2), col = "black", lty = 1, lwd = 1, ...) {
  # hist
  if (result$rank == 1) {

    stop("The method plotAddSpiders() is not applicable to histogram.", call. = FALSE)
  }

  if (result$rank > 1)  {

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("You have to specify 2 axes (e.g., axes = c(1,2)).", call. = FALSE)
    if (max(axes) > result$rank) stop(paste("Specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

    .plot_spiders_internal(result, axes = axes, col = col, lty = lty, lwd = lwd, ...)
  }
}


.plot_spiders_internal <- function(result, axes, col, lty, lwd, ...) {

  result$col = .setValuesForVector(result$objects$Taxon, col)

  scores = data.frame("Taxon" = result$objects$Taxon,
                     "score1" = result$objects$scores[, axes[1]],
                     "score2" = result$objects$scores[, axes[2]],
                     "col" = result$col)

  centroids = data.frame(result$groupMeans$Taxon, result$groupMeans[axes[1]+1], result$groupMeans[axes[2]+1])
  colnames(centroids) = c("Taxon", "cen1", "cen2")


  taxData = merge(scores, centroids, by="Taxon")

  graphics::segments(x0=taxData$cen1, y0=taxData$cen2, x1=taxData$score1 , y1=taxData$score2, col = as.character(taxData$col), lwd = lwd, lty = lty, ...)



}






