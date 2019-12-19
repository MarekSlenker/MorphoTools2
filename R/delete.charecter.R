#' Remove morphological characters from an morphodata object
#'
#' @description This function remove morphological characters from all individuals of morphodata object.
#'
#' @usage delete.charecter(object, charecter)
#'
#' @param object an object of class 'morphodata'.
#' @param charecter vector of characters to be removed.
#'
#' @return object of class 'morphodata'
#'
#' @examples
#' myPartialDataset = delete.charecter(initialdataset, "StemHeight")
#' myPartialDataset = delete.charecter(initialdataset, c("StemHeight", "LeafNo", "PetalLength"))

#' @export

delete.charecter <- function(object, charecter) {
  checkMorphodataClass(object)

  # check existence of CH
  for (ch in charecter) {
    if (! (ch %in% colnames(object$data))) stop(paste("charecter", ch , "does not exist"), call. = F)
  }

  # charecter - moze byt i viac
  toRemove = array(data = NA, dim = 0)
  for (ch in charecter) {
    toRemove = c(toRemove, which(colnames(object$data) == ch) )
  }

  object$data = object$data[ ,-toRemove]

  return(object)
}
