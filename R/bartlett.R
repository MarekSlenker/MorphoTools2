

#' Bartlett Test of Homogeneity of Variances
#' @export
BartlettTest <- function(object) {
  .checkClass(object, "morphodata")


  bt = capture.output(heplots::boxM(object$data, object$Taxon))

  cat("\n Box's M-test for Homogeneity of Covariance Matrices\n", bt[5])

}
