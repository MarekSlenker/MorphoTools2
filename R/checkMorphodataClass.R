checkMorphodataClass <- function(object) {
  if (! (is(object, "morphodata"))) stop("object is not of class 'morphodata'", call. = F)
}
