#' Principal coordinates analysis (PCoA)
#' @export
pcoa.calc <- function(object, distMethod = "euclidean") {
  checkClass(object, "morphodata")

  # miesto toho testujem na NA a vyhodim vynimku
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)

  # find and report constant columns
  #constantColumns = colnames(object$data)[apply(object$data, 2, function(x) (abs(max(x)-min(x)))==0 )]
  #if (length(constantColumns)>0) {
  #  stop(paste("Characters", paste(constantColumns, collapse = ", "), "are constant."), call. = FALSE)
  #}


  pcoaResult = newPcoadata()
  rank = length(colnames(object$data))-1

  princompRes = stats::cmdscale(calcDistance(object, distMethod = distMethod, center = TRUE, scale = TRUE),
                                k = rank, eig = TRUE, x.ret = TRUE)

  newNames = NULL
  for (i in 1:rank) {
    newNames = c(newNames, paste("PCo", i, sep = ""))
  }

  colnames(princompRes$points) = newNames
  #names(princompRes$sdev) = newNames
  #colnames(princompRes$scores) = newNames

  pcoaResult$rank = rank

  pcoaResult$objects$scores = princompRes$points
  pcoaResult$objects$ID = object$ID
  pcoaResult$objects$Population = object$Population
  pcoaResult$objects$Taxon = object$Taxon

  pcoaResult$eigenValues = princompRes$eig
  pcoaResult$eigenvaluesAsPercent = round(princompRes$eig/sum(princompRes$eig), 5)
  pcoaResult$cumulativePercentageOfEigenvalues = round(cumsum(princompRes$eig/sum(princompRes$eig)), 5)

  # group centroid locations
  pcoaResult$groupMeans = stats::aggregate(princompRes$points ~ object$Taxon, FUN = mean)
  colnames(pcoaResult$groupMeans)[1] = "Taxon"

  return(pcoaResult)
}

