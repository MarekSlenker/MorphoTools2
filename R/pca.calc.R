#' Principal component analysis (PCA)
#'
#' @description This function performs principal component analysis.
#'
#' @usage pca.calc(object)
#'
#' @param object an object of class 'morphodata'.
#'
#' @details
#'
#'
#' @return object of class 'prcomp'
#'
#' @examples
#'

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

