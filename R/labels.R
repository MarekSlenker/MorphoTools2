


labels <- function(object, axes = c(1,2), ...) {
  # suitable for "pcadata" or "morphodata", as both stores XY coordinates in $scores
  checkClass(data, c("pcadata", "morphodata"))

  # skontroluj ci axes = 2
  if (length(axes) != 2) stop("you have to specifi 2 axes (e.g., axes = c(1,2))", call. = F)


  text(x = object$objects$scores[ ,axes[1]], y = object$objects$scores[ ,axes[2]],
                         labels = object$objects$ID, ...)


}
