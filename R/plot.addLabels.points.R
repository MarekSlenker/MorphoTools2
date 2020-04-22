#' Add labels to a plot
#' @export
plot.addLabels.points <- function(result, labels = result$objects$ID, include = T, axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...) {
  UseMethod("plot.addLabels.points")
}


#' @rdname plot.addLabels.points
#' @method plot.addLabels.points pcadata
#' @export
plot.addLabels.points.pcadata <- function(pcaResult, labels = pcaResult$objects$ID, include = T, axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...) {

  labels_points_internal(pcaResult, labels, include, axes, pos = pos, offset = offset, cex = cex, col = col, ...)
}

#' @rdname plot.addLabels.points
#' @method plot.addLabels.points cdadata
#' @export
plot.addLabels.points.cdadata <- function(cdaResult, labels = cdaResult$objects$ID, include = T, axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...) {
  # todo ak je rank 1, vyhod chybu  + testuj s rank 1

  if (cdaResult$rank == 1){ stop("Unable to plot labels for histogram", call. = F)  }
  else {
    labels_points_internal(cdaResult, labels, include, axes, pos = pos, offset = offset, cex = cex, col = col, ...)
  }


}


# suitable for "pcadata" or "cdadata", as both stores XY coordinates in $scores
labels_points_internal <- function(object, labels, include, axes, pos = pos, offset = offset, cex = cex, col = col, ...) {
  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (object$rank == 1) stop("this method is not applicable to histograms", call. = F)
  if (max(axes) > object$rank) stop(paste("specified axes are out of bounds. Object has only ", object$rank, " axes.", sep = "" ), call. = F)

  #skontroluj ci labels patria
  # check existence of CH
  for (lab in labels) {
    if (! (lab %in% object$objects$ID)) stop(paste("label", lab , "does not exist"), call. = F)
  }

  labelsToPlot = which(unlist(lapply(object$objects$ID, as.character)) %in% labels)

  if (include) {

    if (length(labelsToPlot) == 0) { stop(paste("No labels to plot"), call. = F) }
    text(x = object$objects$scores[ ,axes[1]][labelsToPlot], y = object$objects$scores[ ,axes[2]][labelsToPlot],
         labels = object$objects$ID[labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)


  } else{

    if (length(labelsToPlot) == length(unlist(lapply(object$objects$ID, as.character))))
        stop(paste("No labels to plot. You specified to exclude (include = FALSE) all labels"), call. = F)

    text(x = object$objects$scores[ ,axes[1]][-labelsToPlot], y = object$objects$scores[ ,axes[2]][-labelsToPlot],
         labels = object$objects$ID[-labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)

  }


}
