

#' Add labels to a plot
#' @export
plotAddLabels.characters <- function(result, labels = characters(result),
                                      include = TRUE, axes = c(1, 2), pos = NULL, offset = 0.5,
                                      cex = 0.7, col = NULL, ...) {
  UseMethod("plotAddLabels.characters")
}


#' @rdname plotAddLabels.characters
#' @method plotAddLabels.characters pcadata
#' @export
plotAddLabels.characters.pcadata <- function(result, labels = characters(result), include = T, axes = c(1,2), pos = NULL, offset = 0.5, cex = 0.7, col = NULL, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specify 2 axes (e.g., axes = c(1,2))", call. = FALSE)
  if (max(axes) > result$rank) stop(paste("specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

  .labels_characters_internal(labelTable = result$eigenVectors, labels = labels, include = include, axes = axes, pos = pos, offset = offset, cex = cex, col = col, ...)
}





#' @rdname plotAddLabels.characters
#' @method plotAddLabels.characters cdadata
#' @export
plotAddLabels.characters.cdadata <- function(result, labels = characters(result), include = T, axes = c(1,2),
                                              pos = NULL, offset = 0.5, cex = 0.7, col = NULL, ...) {

  # hist
  if (result$rank == 1) {

    if (!(all(axes == c(1,2)) ||  (length(axes) == 1  && axes == 1))) warning("The object has only one axis, which will be plotted", call. = FALSE)


    y = seq(length(result$totalCanonicalStructure[,1]), 1, -1)

    for (lab in labels) {
      if (! (lab %in% rownames(result$totalCanonicalStructure))) stop(paste("label", lab , "does not exist."), call. = FALSE)
    }

    labelsToPlot = which(rownames(result$totalCanonicalStructure) %in% labels)


    if (include) {

      if (length(labelsToPlot) == 0) { stop(paste("No labels to plot"), call. = FALSE) }
      graphics::text(x = result$totalCanonicalStructure[,1][labelsToPlot], y = y[labelsToPlot], labels = rownames(result$totalCanonicalStructure)[labelsToPlot],
           pos = pos, offset = offset, cex = cex, col = col, ...)

    } else{

      if (length(labelsToPlot) == length(rownames(result$totalCanonicalStructure))) { stop(paste("No labels to plot. You specified to exclude (include = FALSE) all labels."), call. = FALSE) }
      graphics::text(x = result$totalCanonicalStructure[,1][-labelsToPlot], y = y[-labelsToPlot], labels = rownames(result$totalCanonicalStructure)[-labelsToPlot],
           pos = pos, offset = offset, cex = cex, col = col, ...)

    }
  }


  # scatter
  if (result$rank > 1)  {

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("you have to specify 2 axes (e.g., axes = c(1,2))", call. = FALSE)
    if (max(axes) > result$rank) stop(paste("specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

    .labels_characters_internal(labelTable = result$totalCanonicalStructure, labels = labels, include = include, axes = axes, pos = pos, offset = offset, cex = cex, col = col, ...)
  }
}


.labels_characters_internal <- function(labelTable, labels = labels, include = include, axes = axes, pos = pos, offset = offset, cex = cex, col = col, ...) {

  #skontroluj ci labels patria
  # check existence of CH
  for (lab in labels) {
    if (! (lab %in% rownames(labelTable))) stop(paste("label", lab , "does not exist."), call. = FALSE)
  }

  labelsToPlot = which(rownames(labelTable) %in% labels)

  if (include) {

    if (length(labelsToPlot) == 0) { stop(paste("No labels to plot"), call. = FALSE) }
    graphics::text(x = labelTable[ ,axes[1]][labelsToPlot], y = labelTable[ ,axes[2]][labelsToPlot],
         labels = rownames(labelTable)[labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)

  } else{

    if (length(labelsToPlot) == length(rownames(labelTable))) { stop(paste("No labels to plot. You specified to exclude (include = FALSE) all labels."), call. = FALSE) }
    graphics::text(x = labelTable[ ,axes[1]][-labelsToPlot], y = labelTable[ ,axes[2]][-labelsToPlot],
         labels = rownames(labelTable)[-labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)

  }
}






