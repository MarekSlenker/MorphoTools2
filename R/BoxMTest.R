

#' Box's M-test for Homogeneity of Covariance Matrices
#' @export
BoxMTest <- function(object) {
  .checkClass(object, "morphodata")


  bt = capture.output(heplots::boxM(object$data, object$Taxon))

  cat("Box's M-test for Homogeneity of Covariance Matrices\n", bt[5])
  cat("\n")

}
