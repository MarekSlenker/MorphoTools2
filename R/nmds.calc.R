#' Non-metric multidimensional scaling (NMDS)
#' @export
nmds.calc <- function(object, distMethod = "Euclidean", k = 3, binaryChs = NULL, nominalChs = NULL, ordinalChs = NULL) {
  .checkClass(object, "morphodata")

  # NA niekedy vadia, niekedy nie, zalezitost .calcDistance


  d = .calcDistance(object, distMethod = distMethod, center = TRUE, scale = TRUE, binaryChs = binaryChs, nominalChs = nominalChs, ordinalChs = ordinalChs)


  monoMDSRes = vegan::monoMDS(d, k = k, model = "global", scaling = TRUE, pc = TRUE, weakties = FALSE)

  nmdsResult = .newNmdsdata()



  newNames = NULL
  for (i in 1:k) {
    newNames = c(newNames, paste("axis ", i, sep = ""))
  }

  colnames(monoMDSRes$points) = newNames
  #names(princompRes$sdev) = newNames
  #colnames(princompRes$scores) = newNames

  nmdsResult$rank = k
  nmdsResult$stress = monoMDSRes$stress
  nmdsResult$distMethod = distMethod

  #nmdsResult$diss = monoMDSRes$diss
  #nmdsResult$dist = monoMDSRes$dist


  nmdsResult$objects$scores = monoMDSRes$points
  nmdsResult$objects$ID = object$ID
  nmdsResult$objects$Population = object$Population
  nmdsResult$objects$Taxon = object$Taxon


  # group centroid locations
  nmdsResult$groupMeans = stats::aggregate(monoMDSRes$points ~ object$Taxon, FUN = mean)
  colnames(nmdsResult$groupMeans)[1] = "Taxon"

  return(nmdsResult)

}



