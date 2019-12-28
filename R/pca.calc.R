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
#' \item{scores}{ordination scores of cases (objects, OTUs), centred and scaled. }
#' \item{center, scale}{the centering and scaling used, or FALSE.}
#'
#'
#' \item{eigenVectors}{matrix of eigenvectors (i.e., a matrix of variable loadings)
#'
#'
#'    colour, from D (best) to J (worst)}
#'
#'
#'
#' \item{clarity}{a measurement of how clear the diamond is (I1 (worst), SI2,
#'   SI1, VS2, VS1, VVS2, VVS1, IF (best))}
#' \item{x}{length in mm (0--10.74)}
#' \item{y}{width in mm (0--58.9)}
#' \item{z}{depth in mm (0--31.8)}
#' \item{depth}{total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)}
#' \item{table}{width of top of diamond relative to widest point (43--95)}
#'
#'

#'
#'  eigenVectors the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors).
#' The function princomp returns this in the element loadings.
#'
#'  x if retx is true the value of the rotated data (the centred (and scaled if requested)
#' data multiplied by the rotation matrix) is returned. Hence, cov(x) is the diagonal matrix diag(sdev^2).
#' For the formula method, napredict() is applied to handle the treatment of values omitted by the na.action.
#'
#'  center, scale the centering and scaling used, or FALSE.

#'
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

  pcaResult = newPcadata()

  prcompRes = prcomp(object$data, center=T, scale.=T)
  prcompSummary = summary(prcompRes)

  pcaResult$sdev = prcompRes$sdev
  pcaResult$center = prcompRes$center
  pcaResult$scale = prcompRes$scale

  # taktp je to v Koutecky 2014
  #pcaResult$scores = predict(prcompRes,object$data)
  pcaResult$scores = prcompRes$x


  #pcaResult$eigenVectors = apply(prcompRes$rotation,1,function(x) x*prcompRes$sdev)

  pcaResult$eigenVectors = prcompRes$rotation


  pcaResult$eigenValues = sapply(prcompRes$sdev,function(x) x^2)
  pcaResult$axesVariance = prcompSummary$importance[2,]
  pcaResult$cumulativeAxesVariance = prcompSummary$importance[3,]

  return(pcaResult)
}

