#' Principal coordinates analysis (PCoA)
#' @export
pcoa.calc <- function(object, distMethod = "Euclidean", binaryChs = NULL, nominalChs = NULL, ordinalChs = NULL) {
  .checkClass(object, "morphodata")


  # NA niekedy vadia, niekedy nie, zalezitost .calcDistance
  # find and report constant columns - EE

  pcoaResult = .newPcoadata()
  #rank = min(length(colnames(object$data)), length(rownames(object$data)))-1 # maximum dimension of the space must be in {1, 2, …, n-1}.

  xRes = stats::cmdscale(.calcDistance(object, distMethod = distMethod, center = TRUE, scale = TRUE, binaryChs = binaryChs, nominalChs = nominalChs, ordinalChs = ordinalChs),
                                eig = TRUE) # only to find numb of possitive eigenvalues

  rank = length(xRes$eig[which(xRes$eig > 0)])
  if (rank >= length(object$ID) ) {
    rank = length(object$ID)-1
  }

  cmdscaleRes = stats::cmdscale(.calcDistance(object, distMethod = distMethod, center = TRUE, scale = TRUE, binaryChs = binaryChs, nominalChs = nominalChs, ordinalChs = ordinalChs),
                                k = rank, eig = TRUE, x.ret = TRUE)


  # NAMES: cmdscaleRes$points
  newNames = NULL
  for (i in 1:dim(cmdscaleRes$points)[2]) {
    newNames = c(newNames, paste("PCo", i, sep = ""))
  }
  #cmdscaleRes$points = as.data.frame(cmdscaleRes$points) # update2
  colnames(cmdscaleRes$points) = newNames



  pcoaResult$rank = rank
  pcoaResult$distMethod = distMethod

  pcoaResult$objects$scores = cmdscaleRes$points
  pcoaResult$objects$ID = object$ID
  pcoaResult$objects$Population = object$Population
  pcoaResult$objects$Taxon = object$Taxon

  pcoaResult$eigenvalues = cmdscaleRes$eig #[1:rank]
  pcoaResult$eigenvaluesAsPercentages = round(cmdscaleRes$eig[1:rank]/sum(cmdscaleRes$eig[1:rank]), 5)
  pcoaResult$cumulativePercentageOfEigenvalues = round(cumsum(cmdscaleRes$eig[1:rank]/sum(cmdscaleRes$eig[1:rank])), 5)

  newNames = NULL
  for (i in 1:length(pcoaResult$eigenvaluesAsPercentages)) {
    newNames = c(newNames, paste("PCo", i, sep = ""))
  }
  names(pcoaResult$eigenvaluesAsPercentages) = newNames
  names(pcoaResult$cumulativePercentageOfEigenvalues) = newNames

  # group centroid locations
  pcoaResult$groupMeans = stats::aggregate(cmdscaleRes$points ~ object$Taxon, FUN = mean)

  colnames(pcoaResult$groupMeans)[1] = "Taxon"

  return(pcoaResult)
}

