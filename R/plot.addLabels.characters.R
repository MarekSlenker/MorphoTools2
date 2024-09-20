

#' Add labels to a plot
#' @export
plotAddLabels.characters <- function(result, labels = characters(result),
                                      include = TRUE, axes = c(1, 2), pos = NULL, offset = 0.5,
                                      cex = 0.7, col = NULL, breaks = 1, ...) {
  UseMethod("plotAddLabels.characters")
}


#' @rdname plotAddLabels.characters
#' @method plotAddLabels.characters pcadata
#' @export
plotAddLabels.characters.pcadata <- function(result, labels = characters(result), include = TRUE, axes = c(1,2),
                                             pos = NULL, offset = 0.5, cex = 0.7, col = NULL, breaks = 1, ...) {

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("You have to specify 2 axes (e.g., axes = c(1,2)).", call. = FALSE)
  if (max(axes) > result$rank) stop(paste("Specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

  .labels_characters_internal(labelTable = result$eigenvectors, labels = labels, include = include, axes = axes, pos = pos, offset = offset, cex = cex, col = col, ...)
}





#' @rdname plotAddLabels.characters
#' @method plotAddLabels.characters cdadata
#' @export
plotAddLabels.characters.cdadata <- function(result, labels = characters(result), include = TRUE, axes = c(1,2),
                                              pos = NULL, offset = 0.5, cex = 0.7, col = NULL, breaks = 1, ...) {

  # hist
  if (result$rank == 1) {

    if (!(all(axes == c(1,2)) ||  (length(axes) == 1  && axes == 1))) warning("The object has only one axis, which will be plotted.", call. = FALSE)


    # ORIG
    # y = seq(length(result$totalCanonicalStructure[,1]), 1, -1)

    # Height of plot BEGIN
    taxlev = levels(result$objects$Taxon)
    scores = as.numeric(result$objects$scores[,1])
    xhist = graphics::hist(scores, plot = FALSE)
    hist_breaks = seq(from = min(xhist$breaks), to = max(xhist$breaks), by = breaks )
    # struktura pre skladovanie hystogramov
    histograms = list(list(list(),list(),list(),list(),list(),list()))
    for (i in 1:length(taxlev)) {
      histograms[[i]] = graphics::hist(scores[result$objects$Taxon == taxlev[i]], plot = FALSE, breaks = hist_breaks )
    }
    #   MAX porovnanaj v cykle, na konci cyklu budes mat max zo vsetkych - na nastavien ylim
    ymax = 0
    for (i in 1:length(taxlev)) {
        ymax = max( c(ymax, histograms[[i]]$counts))
    }
    # hrajkanie sa s delenim a zvyskom po delenie, aby som nasiel nablizsie cislo delitelne 10
    upperLim = ymax  %/% 10; if ((ymax %% 10) > 0) upperLim = upperLim + 1; upperLim = upperLim * 10
    ylim = c(0, upperLim)

    # Height of plot END

    y = seq(upperLim*0.9, 1,length.out = length(result$totalCanonicalStructure[,1]))


    for (lab in labels) {
      if (! (lab %in% rownames(result$totalCanonicalStructure))) stop(paste("Label \"", lab , "\" does not exist.", sep = ""), call. = FALSE)
    }



    if (include) {
      labelsToPlot = which(rownames(result$totalCanonicalStructure) %in% labels)
      if (length(labelsToPlot) == 0) { stop(paste("No labels to plot."), call. = FALSE) }

        if (is.null(pos)) {
          # rozhod dprava dolava
          for (ch in labelsToPlot) {
            # hore
            if (result$totalCanonicalStructure[ch] > 0) graphics::text(x = result$totalCanonicalStructure[ch], y = y[ch],
                                                                       labels = rownames(result$totalCanonicalStructure)[ch], cex = cex, pos = 4, offset = offset, col = col, ...)
            # dole
            else graphics::text(x = result$totalCanonicalStructure[ch], y = y[ch],
                                labels = rownames(result$totalCanonicalStructure)[ch], cex = cex, pos = 2, offset = offset, col = col, ...)
          }
        } else {
            graphics::text(x = result$totalCanonicalStructure[,1][labelsToPlot], y = y[labelsToPlot], labels = rownames(result$totalCanonicalStructure)[labelsToPlot],
                 pos = pos, offset = offset, cex = cex, col = col, ...)
        }

    } else { # exclude
      labs = row.names(result$totalCanonicalStructure)[- which( row.names(result$totalCanonicalStructure) %in%  labels)]
      labelsToPlot = which( row.names(result$totalCanonicalStructure) %in%  labs)


      # TODO: if (length(labelsToPlot) == length(rownames(result$totalCanonicalStructure))) { stop(paste("No labels to plot. You specified to exclude (include = FALSE) all labels."), call. = FALSE) }

      if (is.null(pos)){
        # ROZHOD DOPRAVA DOLAVA
        for (ch in labelsToPlot) {
          # hore
          if (result$totalCanonicalStructure[ch] > 0) graphics::text(x = result$totalCanonicalStructure[ch], y = y[ch],
                                                                     labels = rownames(result$totalCanonicalStructure)[ch], cex = cex, pos = 4, offset = offset, col = col, ...)
          # dole
          else graphics::text(x = result$totalCanonicalStructure[ch], y = y[ch],
                              labels = rownames(result$totalCanonicalStructure)[ch], cex = cex, pos = 2, offset = offset, col = col, ...)
        }
      } else {
          graphics::text(x = result$totalCanonicalStructure[,1][labelsToPlot], y = y[labelsToPlot], labels = rownames(result$totalCanonicalStructure)[labelsToPlot],
             pos = pos, offset = offset, cex = cex, col = col, ...)
      }
    }
  }


  # scatter
  if (result$rank > 1)  {

    # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
    if (length(axes) != 2) stop("You have to specify 2 axes (e.g., axes = c(1,2)).", call. = FALSE)
    if (max(axes) > result$rank) stop(paste("Specified axes are out of bounds. Object has only ", result$rank, " axes.", sep = "" ), call. = FALSE)

    .labels_characters_internal(labelTable = result$totalCanonicalStructure, labels = labels, include = include, axes = axes, pos = pos, offset = offset, cex = cex, col = col, ...)
  }
}


.labels_characters_internal <- function(labelTable, labels = labels, include = include, axes = axes, pos = pos, offset = offset, cex = cex, col = col, ...) {

  #skontroluj ci labels patria
  # check existence of CH
  for (lab in labels) {
    if (! (lab %in% rownames(labelTable))) stop(paste("Label \"", lab , "\" does not exist.", sep = ""), call. = FALSE)
  }


  if (include) {
    labelsToPlot = which(rownames(labelTable) %in% labels)

    if (length(labelsToPlot) == 0) { stop(paste("No labels to plot."), call. = FALSE) }

    if (is.null(pos)){
      for (lab in labelsToPlot) {

        # ROZHOD DOSTRAN
        angle = .angle_internal(c(1,0), c(labelTable[ ,axes[1]][lab], labelTable[ ,axes[2]][lab]) )
        if (angle<=45) {
          graphics::text(x = labelTable[ ,axes[1]][lab], y = labelTable[ ,axes[2]][lab],
                         labels = rownames(labelTable)[lab], pos = 4, offset = offset, cex = cex, col = col, ...)
        } else if (angle<=135) {
          if (labelTable[ ,axes[2]][lab]>0){
            graphics::text(x = labelTable[ ,axes[1]][lab], y = labelTable[ ,axes[2]][lab],
                           labels = rownames(labelTable)[lab], pos = 3, offset = offset, cex = cex, col = col, ...)
          } else {
            graphics::text(x = labelTable[ ,axes[1]][lab], y = labelTable[ ,axes[2]][lab],
                           labels = rownames(labelTable)[lab], pos = 1, offset = offset, cex = cex, col = col, ...)
          }
        } else {
          graphics::text(x = labelTable[ ,axes[1]][lab], y = labelTable[ ,axes[2]][lab],
                         labels = rownames(labelTable)[lab], pos = 2, offset = offset, cex = cex, col = col, ...)
        }

        # graphics::text(x = labelTable[ ,axes[1]][labelsToPlot], y = labelTable[ ,axes[2]][labelsToPlot],
        #                labels = rownames(labelTable)[labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)
      }

    }
    else {
        graphics::text(x = labelTable[ ,axes[1]][labelsToPlot], y = labelTable[ ,axes[2]][labelsToPlot],
             labels = rownames(labelTable)[labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)
    }

  } else{ # exclude

    # TODO: if (length(labelsToPlot) == length(rownames(labelTable))) { stop(paste("No labels to plot. You specified to exclude (include = FALSE) all labels."), call. = FALSE) }


    labs = row.names(labelTable)[- which( row.names(labelTable) %in%  labels)]
    labelsToPlot = which( row.names(labelTable) %in%  labs)

    if (is.null(pos)){
      # TODO: ROZHOD DOSTRAN
      for (lab in labelsToPlot) {

        # ROZHOD DOSTRAN
        angle = .angle_internal(c(1,0), c(labelTable[ ,axes[1]][lab], labelTable[ ,axes[2]][lab]) )
        if (angle<=45) {
          graphics::text(x = labelTable[ ,axes[1]][lab], y = labelTable[ ,axes[2]][lab],
                         labels = rownames(labelTable)[lab], pos = 4, offset = offset, cex = cex, col = col, ...)
        } else if (angle<=135) {
          if (labelTable[ ,axes[2]][lab]>0){
            graphics::text(x = labelTable[ ,axes[1]][lab], y = labelTable[ ,axes[2]][lab],
                           labels = rownames(labelTable)[lab], pos = 3, offset = offset, cex = cex, col = col, ...)
          } else {
            graphics::text(x = labelTable[ ,axes[1]][lab], y = labelTable[ ,axes[2]][lab],
                           labels = rownames(labelTable)[lab], pos = 1, offset = offset, cex = cex, col = col, ...)
          }
        } else {
          graphics::text(x = labelTable[ ,axes[1]][lab], y = labelTable[ ,axes[2]][lab],
                         labels = rownames(labelTable)[lab], pos = 2, offset = offset, cex = cex, col = col, ...)
        }

        # graphics::text(x = labelTable[ ,axes[1]][labelsToPlot], y = labelTable[ ,axes[2]][labelsToPlot],
        #                labels = rownames(labelTable)[labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)
      }

      # graphics::text(x = labelTable[ ,axes[1]][labelsToPlot], y = labelTable[ ,axes[2]][labelsToPlot],
      #                labels = rownames(labelTable)[labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)
    } else {
        graphics::text(x = labelTable[ ,axes[1]][-labelsToPlot], y = labelTable[ ,axes[2]][-labelsToPlot],
           labels = rownames(labelTable)[-labelsToPlot], pos = pos, offset = offset, cex = cex, col = col, ...)
      }

  }
}


.angle_internal <- function(x,y){
  dot.prod <- x%*%y
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)*180/pi
}




