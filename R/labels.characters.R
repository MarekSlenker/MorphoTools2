

# generic function


#' @rdname labels.points
#' @export
labels.characters <- function(pcaResult, ...) {
  UseMethod("labels.characters")
}


#' @rdname pca.calc
#' @usage NULL
#' @method labels.characters pcadata
#' @export
labels.characters.pcadata <- function(pcaResult, axes = c(1,2), cex = 0.7, pos = 4, offset = 0.5, ...) {
  checkClass(pcaResult, "pcadata")

  # skontroluj ci axes = 2; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)
  if (max(axes) > pcaResult$rank) stop(paste("specified axes are out of bounds. Object has only ", pcaResult$rank, " axes.", sep = "" ), call. = F)


  text(x = pcaResult$eigenVectors[ ,axes[1]], y = pcaResult$eigenVectors[ ,axes[2]],
       labels = row.names(pcaResult$eigenVectors), cex = cex, pos = pos, offset = offset, ...)
}
