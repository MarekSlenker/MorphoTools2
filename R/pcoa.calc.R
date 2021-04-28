#' Principal coordinates analysis (PCoA)
#' @export
pcoa.calc <- function(object, distMethod = "euclidean") {
  .checkClass(object, "morphodata")


  # NA niekedy vadia, niekedy nie, zalezitost .calcDistance

  # find and report constant columns - EE

  pcoaResult = .newPcoadata()
  rank = min(length(colnames(object$data)), length(rownames(object$data)))-1 # maximum dimension of the space must be in {1, 2, …, n-1}.

  xRes = stats::cmdscale(.calcDistance(object, distMethod = distMethod, center = TRUE, scale = TRUE),
                                eig = TRUE) # only to find numb of possitive eigenvalues
  rank = length(xRes$eig[which(xRes$eig > 0)])

  princompRes = stats::cmdscale(.calcDistance(object, distMethod = distMethod, center = TRUE, scale = TRUE),
                                k = rank, eig = TRUE, x.ret = TRUE)


  newNames = NULL
  for (i in 1:rank) {
    newNames = c(newNames, paste("PCo", i, sep = ""))
  }

  colnames(princompRes$points) = newNames
  names(princompRes$eig) = newNames

  pcoaResult$rank = rank
  pcoaResult$distMethod = distMethod

  pcoaResult$objects$scores = princompRes$points
  pcoaResult$objects$ID = object$ID
  pcoaResult$objects$Population = object$Population
  pcoaResult$objects$Taxon = object$Taxon

  pcoaResult$eigenValues = princompRes$eig[1:rank]
  pcoaResult$eigenvaluesAsPercent = round(princompRes$eig/sum(princompRes$eig[1:rank]), 5)[1:rank]
  pcoaResult$cumulativePercentageOfEigenvalues = round(cumsum(princompRes$eig[1:rank]/sum(princompRes$eig[1:rank])), 5)[1:rank]

  # group centroid locations
  pcoaResult$groupMeans = stats::aggregate(princompRes$points ~ object$Taxon, FUN = mean)
  colnames(pcoaResult$groupMeans)[1] = "Taxon"

  return(pcoaResult)
}

