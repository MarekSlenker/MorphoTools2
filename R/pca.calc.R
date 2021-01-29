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

  princompRes = stats::princomp(object$data, cor = TRUE)

  pcaResult$sdev = princompRes$sdev
  pcaResult$center = princompRes$center
  pcaResult$scale = princompRes$scale
  pcaResult$rank = length(princompRes$sdev)

  # Koutecky 2014
  #pcaResult$scores = predict(princompRes,object$data)
  pcaResult$objects$scores = princompRes$scores
  pcaResult$objects$ID = object$ID
  pcaResult$objects$Population = object$Population
  pcaResult$objects$Taxon = object$Taxon

  # Koutecky 2014
  #pcaResult$eigenVectors = apply(prcompRes$rotation,1,function(x) x*prcompRes$sdev)
  pcaResult$eigenVectors = princompRes$loadings[,1:length(princompRes$sdev)]


  vars <- princompRes$sdev^2
  pcaResult$eigenValues = vars # sapply(princompRes$sdev,function(x) x^2)
  pcaResult$axesVariance = round(vars/sum(vars), 5)
  pcaResult$cumulativeAxesVariance = round(cumsum(vars/sum(vars)), 5)


  # group centroid locations
  pcaResult$groupMeans = stats::aggregate(princompRes$scores ~ object$Taxon, FUN = mean)
  colnames(pcaResult$groupMeans)[1] = "Taxon"

  return(pcaResult)
}

