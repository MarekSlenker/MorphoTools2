#' Principal component analysis (PCA)
#'
#' @description This function performs principal component analysis.
#'
#' @usage pca.calc(object)
#'
#' ## S3 method for class 'pcadata'
#' plot.points(pcaResult, axes = c(1,2), pch = 16, col = "black", labels = FALSE, legend = FALSE, legend.pos = "topright", ncol = 1, ...)
#'
#' ## S3 method for class 'pcadata'
#' plot.3D(pcaResult, axes = c(1,2,3), pch = 16, col = "black", ???? ?????? ??????????????????? ?????????????????????????? labels = FALSE ????????????????????????????????????????? , legend = FALSE, legend.pos = "topright", ncol = 1, ...)
#'
#' ## S3 method for class 'pcadata'
#' plot.characters(pcaResult, axes = c(1,2), col = "red", length = 0.1, xlim = c(-1,1), ylim = c(-1,1), labels = TRUE. ...)
#'
#' ## S3 method for class 'pcadata'
#' labels.points(pcaResult, axes = c(1,2), ...)
#'
#' @param object an object of class 'morphodata'.
#' @param pcaResult object of class 'pcadata'.
#' @param axes x, y axes of plot.
#' @param pch a vector of plotting characters or symbols: see \code{\link{points}}.
#' @param col the colors for points. Multiple colors can be specified so that each Taxon can be given its own color. If there are fewer colors than points they are recycled in the standard fashion.
#' @param labels logical, defines if labels are displayed. Only restricted number of parameters are supported. For more precise labels ploting, use \code{\link{labels.points}} directly.
#' @param legend logical, defines if legend is displayed. Only restricted number of legend parameters are supported. For more precise legend ploting, use \code{\link{plot.legend}} directly.
#' @param legend.pos the x and y co-ordinates or a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.
#' @param ncol the number of columns in which to set the legend items (default is 1, a vertical legend).
#' @param length length of the edges of the arrow head (in inches).
#' @param ... further arguments to be passed to \code{\link{plot.default}}, \code{\link{text}}, \code{\link{arrows}} or other graphical parameters in \code{\link{par}}.
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
#' plot.points(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "red"), pch = c(20, 17, 8, 21), bg = "orange", legend = T, legend.pos = "bottomright", ncol = 2)
#' plot.points(pcaRes, main = "My PCA plot", frame.plot = F, cex = 0.8)
#'
#' plot.3D(_________________________________________________________________)
#'
#' plot.characters(pcaRes, axes = c(1,2))
#'
#' labels.points(pcaRes, axes = c(1,2), cex = 0.8, col = "red", pos = 4, offset = 0.4)
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


  # group centroid locations
  pcaResult$groupMeans = aggregate(prcompRes$x ~ object$Taxon, FUN = mean)
  colnames(pcaResult$groupMeans)[1] = "Taxon"

  return(pcaResult)
}

