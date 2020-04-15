#' Hierarchical clustering
#'
#' @description Hierarchical cluster analysis of .
#'
#' @usage clust(object, method)
#'
#' ## S3 method for class 'hclust'
#' plot(x, hang = 0.1, main = "Cluster Dendrogram", ...)
#'
#' @param object an object of class 'morphodata'.
#' @param method the agglomeration method to be used: "average" (= UPGMA; default), "complete", "ward.D" (= Ward),
#'  "ward.D2", "single", "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#' @param x an object of the type produced by hclust.
#' @param hang	The fraction of the plot height by which labels should hang below the rest of the plot. A negative value will cause the labels to hang down from 0.
#' @param ... further arguments to be passed to \code{\link{plot.default}} or other graphical parameters in \code{\link{par}}.
#'
#' @return object of class 'hclust'
#'
#' @details This functions performs agglomerative hierarchical clustering based on Euclidean distances.
#' Typically, populations are used as OTUs. Characters are standardised to a zero mean and a unit standard deviation.
#'
#' The clustering method should be one of: average (UPGMA; the default), complete or Ward. Note that
#' the other methods are rarely used in morphometric analyses.
#'
#' Dendrograms are produced using plot function.
#'
#' @examples
#' clustering.UPGMA = clust(data, method = "average")
#'
#' plot(clustering.UPGMA, cex=0.6, cex.lab=0.8, frame.plot=T, hang=-0.1, main="", xlab="", ylab="distance")
#'
#' @export
clust <- function(object, method = "average") {
  checkClass(object, "morphodata")

  object$data = scale(object$data)
  distances = dist(object$data, method = "euclidean")

  # v parametri method mozme dostat akukolvek metodu, ktora je platna pre hclust
  if (method=="UPGMA") method = "average"
  if (method=="ward") method = "ward.D"
  if (method=="WPGMA") method = "mcquitty"
  if (method=="WPGMC") method = "median"
  if (method=="UPGMC") method = "centroid"

  clustering = hclust(distances, method = method)

  return(clustering)
}

