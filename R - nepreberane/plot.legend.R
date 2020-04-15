# generic function


#' Add legends to plot
#'
#' @description This function can be used to add legends to plots.
#'
#' @usage plot.legend(result, x = "topright", y = NULL, axes = c(1,2), pch = 16, col = "black", ...)
#'
#' @param result 	an results previously ploted.
#' @param x,y  the x and y co-ordinates or a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.
#' @param col the color of points appearing in the legend.
#' @param pch the  plotting symbols of points appearing in the legend.
#' @param pt.bg the background color for the \code{\link{points}}, corresponding to its argument bg.
#' @param ... further arguments to be passed to \code{\link{legend}} or other graphical parameters in \code{\link{par}}.
#'
#' @export
plot.legend <- function(result, ...) {
  UseMethod("plot.legend")
}


#' @rdname plot.legend
#' @method plot.legend pcadata
#' @export
plot.legend.pcadata <- function(pcaResult, x = "topright", y = NULL, pch = 16, col = "black", pt.bg = "white", ...) {

  checkClass(pcaResult, "pcadata")

  plot_legend_internal(pcaResult, x = x, y = y, pch = pch, col = col, pt.bg = pt.bg, ...)

}

#' @rdname plot.legend
#' @method plot.legend cdadata
#' @export
plot.legend.cdadata <- function(cdaResult, x = "topright", y = NULL, pch = 16, col = "black", pt.bg = "white", ...) {

  checkClass(cdaResult, "cdadata")

  plot_legend_internal(cdaResult, x = x, y = y, pch = pch, col = col, pt.bg = pt.bg, ...)

}

plot_legend_internal <- function(object, x, y, pch, col, pt.bg, ...) {
  # nastav pch a col spravne podla taxonu
  object$pch = as.numeric( setValuesForVector(object$objects$Taxon, pch))
  object$col = setValuesForVector(object$objects$Taxon, col)
  object$pt.bg = setValuesForVector(object$objects$Taxon, pt.bg)


  legendTable = cbind(as.character(object$objects$Taxon), object$pch, object$col, object$pt.bg)
  legendTable = unique(legendTable)

  plotLegend(legend.pos = c(x, y), legend = legendTable[,1],  pch = as.numeric(legendTable[,2]), col = legendTable[,3], pt.bg = legendTable[,4], ...)
}
