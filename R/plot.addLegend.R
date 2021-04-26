#' Add legends to plot
#' @export
plotAddLegend <- function(result, x = "topright", y = NULL, pch = 16, col = "black",
                           pt.bg = "white", pt.cex = cex, pt.lwd = 1, x.intersp = 1,
                           y.intersp = 1, box.type = "o", box.lty = "solid", box.lwd = 1,
                           box.col = "black", box.bg = "white", cex = 1, ncol = 1, horiz = FALSE, ...) {
  UseMethod("plotAddLegend")
}


#' @rdname plotAddLegend
#' @method plotAddLegend pcadata
#' @export
plotAddLegend.pcadata <- function(result, x = "topright", y = NULL, pch = 16, col = "black",
                            pt.bg = "white", pt.cex = cex, pt.lwd = 1, x.intersp = 1,
                            y.intersp = 1, box.type = "o", box.lty = "solid", box.lwd = 1,
                            box.col = "black", box.bg = "white", cex = 1, ncol = 1, horiz = FALSE, ...) {


  plot_legend_internal(result, x = x, y = y, pch = pch, col = col,
                       pt.bg = pt.bg, pt.cex = pt.cex, pt.lwd = pt.lwd, x.intersp = x.intersp,
                        y.intersp = y.intersp, bty = box.type, box.lty = box.lty, box.lwd = box.lwd,
                       box.col = box.col, bg = box.bg, cex = cex, ncol = ncol, horiz = horiz, ...)

}

#' @rdname plotAddLegend
#' @method plotAddLegend pcoadata
#' @export
plotAddLegend.pcoadata <- function(result, x = "topright", y = NULL, pch = 16, col = "black",
                                  pt.bg = "white", pt.cex = cex, pt.lwd = 1, x.intersp = 1,
                                  y.intersp = 1, box.type = "o", box.lty = "solid", box.lwd = 1,
                                  box.col = "black", box.bg = "white", cex = 1, ncol = 1, horiz = FALSE, ...) {


  plot_legend_internal(result, x = x, y = y, pch = pch, col = col,
                       pt.bg = pt.bg, pt.cex = pt.cex, pt.lwd = pt.lwd, x.intersp = x.intersp,
                       y.intersp = y.intersp, bty = box.type, box.lty = box.lty, box.lwd = box.lwd,
                       box.col = box.col, bg = box.bg, cex = cex, ncol = ncol, horiz = horiz, ...)

}

#' @rdname plotAddLegend
#' @method plotAddLegend cdadata
#' @export
plotAddLegend.cdadata <- function(result, x = "topright", y = NULL, pch = 16, col = "black",
                                   pt.bg = "white", pt.cex = cex, pt.lwd = 1, x.intersp = 1,
                                   y.intersp = 1, box.type = "o", box.lty = "solid", box.lwd = 1,
                                   box.col = "black", box.bg = "white", cex = 1, ncol = 1, horiz = FALSE, ...) {

  plot_legend_internal(result, x = x, y = y, pch = pch, col = col,
                        pt.bg = pt.bg, pt.cex = pt.cex, pt.lwd = pt.lwd, x.intersp = x.intersp,
                        y.intersp = y.intersp, bty = box.type, box.lty = box.lty, box.lwd = box.lwd,
                        box.col = box.col, bg = box.bg, cex = cex, ncol = ncol, horiz = horiz, ...)

}

plot_legend_internal <- function(object, x, y, pch, col, pt.bg, pt.cex, pt.lwd, x.intersp, y.intersp, bty, box.lty, box.lwd, box.col, bg, cex, ncol, horiz, ...) {
  # nastav pch a col spravne podla taxonu
  object$pch = as.numeric( setValuesForVector(object$objects$Taxon, pch))
  object$col = setValuesForVector(object$objects$Taxon, col)
  object$pt.bg = setValuesForVector(object$objects$Taxon, pt.bg)


  legendTable = cbind(as.character(object$objects$Taxon), object$pch, object$col, object$pt.bg )
  legendTable = unique(legendTable)
  colnames(legendTable) = c("Taxon", "pch", "col", "pt.bg")

  legendTable = legendTable[order(legendTable[,1]),]

  if (is.null(y) && x %in% c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"))
    graphics::legend(x, legend = legendTable[,1],
                     pch = as.numeric(legendTable[,2]),
                     col = legendTable[,3],
                     pt.bg = legendTable[,4],
                     pt.cex = pt.cex,
					           pt.lwd = pt.lwd,
                     x.intersp = x.intersp, y.intersp = y.intersp,
                     bty = bty,
					           box.lty = box.lty, box.lwd = box.lwd, box.col = box.col, bg = bg,
                     cex = cex, ncol = ncol, horiz = horiz, ...)

  if (is.numeric(x) && is.numeric(y))
    graphics::legend(x, y, legend = legendTable[,1],
                     pch = as.numeric(legendTable[,2]),
                     col = legendTable[,3],
                     pt.bg = legendTable[,4],
                     pt.cex = pt.cex,
			               pt.lwd = pt.lwd,
                     x.intersp = x.intersp, y.intersp = y.intersp, bty = bty,
					 box.lty = box.lty, box.lwd = box.lwd, box.col = box.col, bg = bg,
					 cex = cex, ncol = ncol, horiz = horiz, ...)

}


