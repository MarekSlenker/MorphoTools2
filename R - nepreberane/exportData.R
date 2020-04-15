

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

  if (n > object$rank) stop(paste("specified 'n' is out of bounds. Object has only ", object$rank, " eigenvectors", sep = "" ), call. = F)

  return(object$eigenVectors[,1:n])
}

#' @rdname pca.eigenVectors
#' @export
pca.eigenValues <- function(object, n = 4) {
  checkClass(object, "pcadata")

  if (n > object$rank) stop(paste("specified 'n' is out of bounds. Object has only ", object$rank, " eigenvalues", sep = "" ), call. = F)

  return(object$eigenValues[1:n])
}


#' Export CDA eigenvectors & total canonical structure
#'
#' @description These functions simplifies exporting eigenvalues and total canonical structure coeficients from 'cdadata' object.
#'
#' @usage
#' cda.eigenValues(object)
#' cda.totalCanonicalStructure(object)
#'
#' @param object 	an object of class 'cdadata'.
#'
#' @examples
#' eVals = cda.eigenValues(myCdaResult)
#' tcs = cda.totalCanonicalStructure(myCdaResult)
#'
#' export.res(eVals, file = "./eigenValues.txt")
#'
#' @export
cda.eigenValues <- function(object) {
  checkClass(object, "cdadata")

  return(object$eigenValues)
}

#' @rdname cda.eigenValues
#' @export
cda.totalCanonicalStructure <- function(object) {
  checkClass(object, "cdadata")

  return(object$totalCanonicalStructure)
}
