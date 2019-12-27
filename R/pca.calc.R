#' Principal component analysis (PCA)
#'
#' @description This function performs principal component analysis.
#'
#' @usage pca.calc(object)
#'
#' @param object an object of class 'morphodata'.
#'
#' @details Generally, most of the multivariate analyses require a full data matrix.
#' The preferred approach is to reduce the data set to complete observations only (i.e. perform the casewise deletion of
#'
#' @return object of class 'prcomp'
#'
#' @examples
#' myCompleteDataset = na.meansubst(initialdataset)

#' @export
pca.calc <- function(object) {
  checkMorphodataClass(object)

  # xdata<-na.omit(DATA)
  # toto nerobim, nebudem uzivatelovi nic z jeho dat vyhadzovat. nech si to vyrisi sam

  # miesto toho testujem na NA a vyhodim vynimku
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)

  pcaResult = prcomp(object$data, center=T, scale.=T)
  return(pcaResult)
}

