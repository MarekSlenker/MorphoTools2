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
#' @return object of class 'pcadata' containing the following components:
#'
#' \item{sdev}{the standard deviations of the principal components (i.e., the square roots of the eigenvalues). }
#' \item{center, scale}{the centering and scaling used, or FALSE.}
#' \item{objects}{containing ID, Population, Taxon, and ordination scores of cases (objects, OTUs), centred and scaled.}
#' \item{eigenVectors}{matrix of eigenvectors (i.e., a matrix of characters loadings).}
#' \item{eigenValues}{eigenvalues of principal components.}
#' \item{axesVariance}{eigenvalues as percent. }
#' \item{cumulativeAxesVariance}{cumulative percentage of eigenvalues. }
#'
#' @examples
#' pcaResult = pca.calc(myMorphoData)
#'
#' @export
pca.calc <- function(object) {
  checkClass(object, "morphodata")

  # xdata<-na.omit(DATA)
  # toto nerobim, nebudem uzivatelovi nic z jeho dat vyhadzovat. nech si to vyrisi sam

  # miesto toho testujem na NA a vyhodim vynimku
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)

  pcaResult = newPcadata()

  prcompRes = prcomp(object$data, center=T, scale.=T)
  prcompSummary = summary(prcompRes)

  pcaResult$sdev = prcompRes$sdev
  pcaResult$center = prcompRes$center
  pcaResult$scale = prcompRes$scale

  # Koutecky 2014
  #pcaResult$scores = predict(prcompRes,object$data)
  pcaResult$objects$scores = prcompRes$x
  pcaResult$objects$ID = object$ID
  pcaResult$objects$Population = object$Population
  pcaResult$objects$Taxon = object$Taxon

  # Koutecky 2014
  #pcaResult$eigenVectors = apply(prcompRes$rotation,1,function(x) x*prcompRes$sdev)
  pcaResult$eigenVectors = prcompRes$rotation

  pcaResult$eigenValues = sapply(prcompRes$sdev,function(x) x^2)
  pcaResult$axesVariance = prcompSummary$importance[2,]
  pcaResult$cumulativeAxesVariance = prcompSummary$importance[3,]

  return(pcaResult)
}

