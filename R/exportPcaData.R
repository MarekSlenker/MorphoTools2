

#' Export PCA eigenvectors & eigenvalues
#'
#' @description These functions simplifies exporting eigenvectors and eigenvalues from 'pcadata' object.
#'
#' @usage
#' pca.eigenVectors(object, n = 4)
#' pca.eigenValues(object, n = 4)
#'
#' @param object 	an object of class 'pcadata'.
#' @param n number of axes to export
#'
#' @examples
#' eVecs = pca.eigenVectors(myPcaResult)
#' eVals = pca.eigenValues(myPcaResult)
#'
#' export.res(eVecs, file = "./eigenVectors.txt")
#'
#' @export
pca.eigenVectors <- function(object, n = 4) {
  checkClass(object, "pcadata")

  return(object$eigenVectors[,1:n])
}

#' @rdname pca.eigenVectors
#' @export
pca.eigenValues <- function(object, n = 4) {
  checkClass(object, "pcadata")

  return(object$eigenValues[1:n])
}
