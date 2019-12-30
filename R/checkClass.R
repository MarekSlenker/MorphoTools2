checkClass <- function(object, class) {
  if (! (is(object, class))) stop(paste("object is not of class '", class, "' ", sep = ""), call. = F)
}
