#' The default scatterplot 3D function
#' @export
plot3Dpoints <- function(result, axes = c(1, 2, 3), xlab = NULL, ylab = NULL,
                          zlab = NULL, pch = 16, col = "black", pt.bg = "white",
                          phi = 10, theta = 2, ticktype = "detailed", bty = "u",
                          type = "p", labels = FALSE, legend = FALSE, legend.pos
                          = "topright", ncol = 1, ...) {
  UseMethod("plot3Dpoints")
}


#' @rdname plot3Dpoints
#' @method plot3Dpoints pcadata
#' @export
plot3Dpoints.pcadata <- function(result, axes = c(1,2,3), xlab = NULL, ylab = NULL, zlab = NULL,
         pch = 16, col = "black", pt.bg = "white", phi = 10, theta = 2,
         ticktype = "detailed", bty = "u", type = "p", labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

  if (is.null(xlab)) xlab = paste(names(result$eigenValues)[axes[1]]," (", round(result$eigenvaluesAsPercent[axes[1]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(ylab)) ylab = paste(names(result$eigenValues)[axes[2]]," (", round(result$eigenvaluesAsPercent[axes[2]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(zlab)) zlab = paste(names(result$eigenValues)[axes[3]]," (", round(result$eigenvaluesAsPercent[axes[3]]*100, digits = 2) ,"%)", sep = "")


  plot3D_internal(result, axes = axes, xlab = xlab, ylab = ylab, zlab = zlab,
                  pch = pch, col = col, pt.bg = pt.bg, phi = phi, theta = theta,
                  ticktype = ticktype, bty = bty, type = type,
                  labels = labels, legend = legend, legend.pos = legend.pos, ncol = ncol, ...)
}


#' @rdname plot3Dpoints
#' @method plot3Dpoints pcoadata
#' @export
plot3Dpoints.pcoadata <- function(result, axes = c(1,2,3), xlab = NULL, ylab = NULL, zlab = NULL,
                                 pch = 16, col = "black", pt.bg = "white", phi = 10, theta = 2,
                                 ticktype = "detailed", bty = "u", type = "p", labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

  if (is.null(xlab)) xlab = paste(names(result$eigenValues)[axes[1]]," (", round(result$eigenvaluesAsPercent[axes[1]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(ylab)) ylab = paste(names(result$eigenValues)[axes[2]]," (", round(result$eigenvaluesAsPercent[axes[2]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(zlab)) zlab = paste(names(result$eigenValues)[axes[3]]," (", round(result$eigenvaluesAsPercent[axes[3]]*100, digits = 2) ,"%)", sep = "")


  plot3D_internal(result, axes = axes, xlab = xlab, ylab = ylab, zlab = zlab,
                  pch = pch, col = col, pt.bg = pt.bg, phi = phi, theta = theta,
                  ticktype = ticktype, bty = bty, type = type,
                  labels = labels, legend = legend, legend.pos = legend.pos, ncol = ncol, ...)
}


#' @rdname plot3Dpoints
#' @method plot3Dpoints cdadata
#' @export
plot3Dpoints.cdadata <- function(result, axes = c(1,2,3), xlab = NULL, ylab = NULL, zlab = NULL,
        pch = 16, col = "black", pt.bg = "white", phi = 10, theta = 2,
        ticktype = "detailed", bty = "u", type = "p", labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...) {

  if (result$rank < 3) stop(paste("3D plot requires at least 3 axes. Object has ", result$rank, " axes.", sep = "" ), call. = FALSE)

  if (is.null(xlab)) xlab = paste(names(result$eigenValues)[axes[1]]," (", round(result$eigenvaluesAsPercent[axes[1]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(ylab)) ylab = paste(names(result$eigenValues)[axes[2]]," (", round(result$eigenvaluesAsPercent[axes[2]]*100, digits = 2) ,"%)", sep = "")
  if (is.null(zlab)) zlab = paste(names(result$eigenValues)[axes[3]]," (", round(result$eigenvaluesAsPercent[axes[3]]*100, digits = 2) ,"%)", sep = "")


  plot3D_internal(result, axes = axes, xlab = xlab, ylab = ylab, zlab = zlab,
                  pch = pch, col = col, pt.bg = pt.bg, phi = phi, theta = theta,
                  ticktype = ticktype, bty = bty, type = type,
                  labels = labels, legend = legend, legend.pos = legend.pos, ncol = ncol, ...)
}


plot3D_internal <- function(object, axes = axes, xlab = xlab, ylab = ylab, zlab = zlab,
                            pch = pch, col = col, pt.bg = pt.bg, phi = phi, theta = theta,
                            ticktype = ticktype, bty = bty, type = type,
                            labels = labels, legend = legend, legend.pos = legend.pos, ncol = ncol, ...) {

  # skontroluj ci axes = 3; a ci uzivatel nezadal cislo osi mimo rozsahu
  if (length(axes) != 3) stop("you have to specify 3 axes (e.g., axes = c(1,2,3))", call. = FALSE)
  if (max(axes) > object$rank) stop(paste("specified axes are out of bounds. Object has only ", object$rank, " axes.", sep = "" ), call. = FALSE)


  # nastav pch a col spravne podla taxonu
  object$pch = as.numeric( setValuesForVector(object$objects$Taxon, pch))
  object$col = setValuesForVector(object$objects$Taxon, col)
  object$pt.bg = setValuesForVector(object$objects$Taxon, pt.bg)

  # main plot
  plot3D::scatter3D(x = object$objects$scores[ ,axes[1]], y = object$objects$scores[ ,axes[2]], z = object$objects$scores[ ,axes[3]],
            colvar = NULL, colkey = T, col = object$col, pch = object$pch, bg.col = object$pt.bg,
            phi = phi, theta = theta, ticktype = ticktype, bty = bty, type = type, xlab = xlab, ylab = ylab, zlab = zlab, ...)


  # legend
  if (legend) {

    plotAddLegend(object, x = legend.pos, pch = pch, col = col, pt.bg = pt.bg, ncol = ncol)

   }


  # labels
  {
    if (labels == T)
      plot3D::text3D(x = object$objects$scores[ ,axes[1]], y = object$objects$scores[ ,axes[2]], z = object$objects$scores[ ,axes[3]],
             labels  = object$objects$ID,
             add = TRUE, cex = 0.8)
  }





}






