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
cda.calc <- function(object, passiveSamples = NULL) {
  checkClass(object, "morphodata")

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)


  # vypocitaj na zaklade skratenej matice (bez pop alebo taxa)
  # ak NULL, matica sa nezmeni

  objectNoPassiveSamples = object
  objectWithPassiveSamples = object

  for (groupName in passiveSamples) {
    if (groupName %in% object$Taxon) objectNoPassiveSamples = removeByColumn(objectNoPassiveSamples, "Taxon", groupName)
    if (groupName %in% object$Population) objectNoPassiveSamples = removeByColumn(objectNoPassiveSamples, "Population", groupName)
  }


  # calculate with objectNoPassiveSamples
  d = as.matrix(objectNoPassiveSamples$data)
  x = lm(d ~ objectNoPassiveSamples$Taxon)
  cda = candisc(x, term="objectNoPassiveSamples$Taxon")



  cdaResult = newCdadata()

  cdaResult$rank = cda$rank
  cdaResult$eigenValues = cda$eigenvalues
  cdaResult$canrsq = cda$canrsq
  cdaResult$axesVariance = cda$pct / 100

  for (i in cdaResult$rank:1) {
    cdaResult$cumulativeAxesVariance[i] = sum(cdaResult$axesVariance[1:i])
  }

  cdaResult$coeffs.raw = cda$coeffs.raw
  cdaResult$coeffs.std = cda$coeffs.std
  cdaResult$totalCanonicalStructure = cda$structure


  cdaResult$objects$ID = object$ID
  cdaResult$objects$Population = object$Population
  cdaResult$objects$Taxon = object$Taxon

  # predict na zaklade plnej matice
  cdaResult$objects$scores = scale(objectWithPassiveSamples$data, center = T, scale = F) %*% cda$coeffs.raw

  #  predict na zaklade plnej matice = novych dat
  cdaResult$groupMeans = aggregate(cdaResult$objects$scores, by = list("Taxon" = cdaResult$objects$Taxon), mean)

  return(cdaResult)
}
