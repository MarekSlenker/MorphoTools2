#' Canonical discriminant analysis (CDA)
#'
#' @description This function performs generalized canonical discriminant analysis.
#'
#' @usage cda.calc(object)
#'
#' @param object an object of class 'morphodata'.
#'
#' @details
#'
#'
#' @return object of class 'cdadata' containing the following components:
#'
#' \item{totalCanonicalStructure}{totalCanonicalStructuretotalCanonicalStructuretotalCanonicalStructuretotalCanonicalStructure}
#'
#' @examples
#' cdaResult = cda.calc(myMorphoData)
#'
#' @export
cda.calc <- function(object) {
  checkClass(object, "morphodata")

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)

  # calculate
  d = as.matrix(object$data)
  x <- lm(d ~ object$Taxon)
  cda <- candisc(x, term="object$Taxon")


  cdaResult = newCdadata()

  cdaResult$rank = cda$rank
  cdaResult$eigenValues = cda$eigenvalues
  cdaResult$canrsq = cda$canrsq
  cdaResult$axesVariance = cda$pct
  cdaResult$groupMeans = cda$means
  cdaResult$coeffs.raw = cda$coeffs.raw
  cdaResult$coeffs.std = cda$coeffs.std
  cdaResult$totalCanonicalStructure = cda$structure

  cdaResult$objects$ID = object$ID
  cdaResult$objects$Population = object$Population
  cdaResult$objects$Taxon = object$Taxon

  cdaResult$objects$scores = cda$scores
  colnames(cdaResult$objects$scores)[1] = "Taxon"

  return(cdaResult)
}
