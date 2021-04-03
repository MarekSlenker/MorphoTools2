# #' @importFrom stats qqnorm
# #' @export

#' Quantile-Quantile Plots
#' @export
qqnorm.morphodata <- function(object, character, ...) {


  hist(c(rnorm(30), rnorm(30)+4))

  qqnorm((rnorm(30)))

  qqnorm(sqrt(cent.orig$LBA))
  qqline(sqrt(cent.orig$SF))




}
