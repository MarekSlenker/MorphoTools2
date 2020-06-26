#' Principal component analysis (PCA)
#' @export
pca.calc <- function(object) {
  checkClass(object, "morphodata")

  # xdata<-na.omit(DATA)
  # toto nerobim, nebudem uzivatelovi nic z jeho dat vyhadzovat. nech si to vyrisi sam

  # miesto toho testujem na NA a vyhodim vynimku
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)

  # find and report constant columns
  constantColumns = colnames(object$data)[apply(object$data, 2, function(x) (abs(max(x)-min(x)))==0 )]
  if (length(constantColumns)>0) {
    stop(paste("Characters", paste(constantColumns, collapse = ", "), "are constant."), call. = FALSE)
  }


  pcaResult = newPcadata()

  prcompRes = prcomp(object$data, center=T, scale.=T)
  prcompSummary = summary(prcompRes)

  pcaResult$sdev = prcompRes$sdev
  pcaResult$center = prcompRes$center
  pcaResult$scale = prcompRes$scale
  pcaResult$rank = length(prcompRes$sdev)

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

