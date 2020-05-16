#' Hierarchical clustering
#' @export
clust <- function(object, distMethod = "euclidean", clustMethod = "average") {
  checkClass(object, "morphodata")

  supportedDistMethods = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  supportedClustMethods = c("ward.D", "ward.D2", "ward", "single", "complete", "average", "UPGMA",
                            "mcquitty", "WPGMA", "median", "WPGMC", "centroid", "UPGMC")


  # skontroluj argumenty
  if (! (distMethod %in% supportedDistMethods)) stop(paste("distMethod", distMethod , "is not supported."), call. = F)
  if (! (clustMethod %in% supportedClustMethods)) stop(paste("clustMethod", clustMethod , "is not supported."), call. = F)

  if (any(is.na(object$data))) warning("Values of some characters are NA.", call. = FALSE)

  object$data = scale(object$data)
  distances = dist(object$data, method = distMethod)

  # v parametri method mozme dostat akukolvek metodu, ktora je platna pre hclust
  if (clustMethod == "UPGMA") clustMethod = "average"
  if (clustMethod == "ward") clustMethod = "ward.D"
  if (clustMethod == "WPGMA") clustMethod = "mcquitty"
  if (clustMethod == "WPGMC") clustMethod = "median"
  if (clustMethod == "UPGMC") clustMethod = "centroid"

  clustering = hclust(distances, method = clustMethod)

  return(clustering)
}

