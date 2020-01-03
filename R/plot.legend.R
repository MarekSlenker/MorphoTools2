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
#' @param ... further arguments to be passed to \code{\link{legend}} or other graphical parameters in \code{\link{par}}.
#'
#' @export
plot.legend <- function(result, ...) {
  UseMethod("plot.legend")
}


#' @rdname plot.legend
#' @method plot.legend pcadata
#' @export
plot.legend.pcadata <- function(result, x = "topright", y = NULL, pch = 16, col = "black", ...) {

  checkClass(result, "pcadata")

  plot.legend.internal(result, x = x, y = y, pch = pch, col = col, ...)

}

plot.legend.internal <- function(result, x, y, pch, col, ...) {
  # nastav pch a col spravne podla taxonu
  result$pch = as.numeric( setValuesForVector(result$objects$Taxon, pch))
  result$col = setValuesForVector(result$objects$Taxon, col)


  legend(x, y, legend = unique(result$objects$Taxon),
               pch = unique(result$pch),
               col = unique(result$col),
               ...)
}
