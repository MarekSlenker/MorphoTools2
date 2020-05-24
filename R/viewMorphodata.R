#' Invoke a Data Viewer
#' @export
viewMorphodata <- function(object) {
  checkClass(object, "morphodata")

  utils::View(object$data)
}
