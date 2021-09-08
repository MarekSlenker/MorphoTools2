#' Box's M-test for Homogeneity of Covariance Matrices
#' @export
boxMTest <- function(object) {
  .checkClass(object, "morphodata")


  bt = utils::capture.output(heplots::boxM(object$data, object$Taxon))

  cat("Box's M-test for homogeneity of covariance matrices\n", bt[5])
  cat("\n")

}
