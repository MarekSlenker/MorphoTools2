

#' Bartlett Test of Homogeneity of Variances
#' @export
BartlettTest <- function(object) {
  .checkClass(object, "morphodata")


  bt = bartlett.test(object$data, object$Taxon)
  cat("
      Bartlett test of homogeneity of variances
      ")
  cat("Bartlett's K-squared = 65985, df = 24, p-value < 2.2e-16")







plot(count ~ spray, data = InsectSprays)
bartlett.test(InsectSprays$count, InsectSprays$spray)
bartlett.test(count ~ spray, data = InsectSprays)


data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))


object = centaurea

}
