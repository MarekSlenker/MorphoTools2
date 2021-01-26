#' Add labels to a plot
#' @export
plotAddLabels.points <- function(result, labels = result$objects$ID, include = TRUE, axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...) {
  UseMethod("plotAddLabels.points")
}


#' @rdname plotAddLabels.points
#' @method plotAddLabels.points pcadata
#' @export
plotAddLabels.points.pcadata <- function(result, labels = result$objects$ID, include = TRUE, axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...) {

  labels_points_internal(result, labels, include, axes, pos = pos, offset = offset, cex = cex, col = col, ...)
}

#' @rdname plotAddLabels.points
#' @method plotAddLabels.points cdadata
#' @export
plotAddLabels.points.cdadata <- function(result, labels = result$objects$ID, include = TRUE, axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...) {
  # todo ak je rank 1, vyhod chybu  + testuj s rank 1

  if (result$rank == 1){ stop("Unable to plot labels for histogram", call. = FALSE)  }
  else {
    labels_points_internal(result, labels, include, axes, pos = pos, offset = offset, cex = cex, col = col, ...)
  }


}


# suitable for "pcadata" or "cdadata", as both stores XY coordinates in $scores
labels_points_internal <- function(object, labels, include, axes, pos = pos, offset = offset, cex = cex, col = col, ...) {
  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = FALSE)
  if (object$rank == 1) stop("this method is not applicable to histograms", call. = FALSE)
  if (max(axes) > object$rank) stop(paste("specified axes are out of bounds. Object has only ", object$rank, " axes.", sep = "" ), call. = FALSE)

  #skontroluj ci labels patria
  # check existence of CH
  for (lab in labels) {
    if (! (lab %in% object$objects$ID)) stop(paste("label", lab , "does not exist"), call. = F)
  }

  labelsToPlot = which(unlist(lapply(object$objects$ID, as.character)) %in% labels)

  if (include) {

    if (length(labelsToPlot) == 0) { stop(paste("No labels to plot"), call. = FALSE) }
    graphics::text(x = object$objects$scores[ ,axes[1]][labelsToPlot], y = object$objects$scores[ ,axes[2]][labelsToPlot],
         labels = object$objects$ID[labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)


  } else{

    if (length(labelsToPlot) == length(unlist(lapply(object$objects$ID, as.character))))
        stop(paste("No labels to plot. You specified to exclude (include = FALSE) all labels"), call. = FALSE)

    graphics::text(x = object$objects$scores[ ,axes[1]][-labelsToPlot], y = object$objects$scores[ ,axes[2]][-labelsToPlot],
         labels = object$objects$ID[-labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)

  }


}
