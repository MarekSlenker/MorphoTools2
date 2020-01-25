#' Summarize a structure of a morphodata object.
#'
#' @description This function summarizes the information contained in the a morphodata object.
#' @usage
#' ## S3 method for class 'morphodata'
#' summary(object)
#' @param object an object of class 'morphodata'.
#' @examples
#' summary(object)
#' @export
summary.morphodata <- function(object) {
  #  is of morphodata class?  (keby tuto genericku metodu chel pouzit niekto priamo)
  checkClass(object, "morphodata")

  # cat summary

  cat("object of class 'morphodata'\n")
  cat(paste(" - contains ", length(levels(object$Population)), " populations\n", sep = ""))
  cat(paste(" - contains ", length(levels(object$Taxon)), " defined groups (taxa)\n", sep = ""))
  cat("\n")
  cat(paste("* Populations: ", paste(levels(object$Population), collapse = ", "), "\n", sep = ""))
  cat(paste("* Groups (taxa): ", paste(levels(object$Taxon), collapse = ", "), "\n", sep = ""))
}




#' Summarize a structure of a pcadata object.
#'
#' @description This function summarizes the information contained in the a pcadata object.
#' @usage
#' ## S3 method for class 'pcadata'
#' summary(object)
#' @param object an object of class 'pcadata'.
#' @examples
#' summary(object)
#' @export
summary.pcadata <- function(object) {
  checkClass(object, "pcadata")

  cat("object of class 'pcadata'; storing original data and results of Principal Component Analysis\n")
  cat("\nVariation explained by individual axes:\n")

  descrTable = data.frame(row.names = names(object$axesVariance),
                          "Standard deviation" = round(object$sdev, digits = 4),
                          "Proportion of Variance" = round(object$axesVariance, digits = 4),
                          "Cumulative Proportion" = round(object$cumulativeAxesVariance, digits = 4)
                          )
  names(descrTable) = gsub(pattern = '\\.' , replacement = " ", x = names(descrTable))
  descrTable = t(descrTable)

  print(descrTable)

  cat("\nThe correlation of the characters and ordination axes:\n")

  print(object$eigenVectors[,1:4])

}













