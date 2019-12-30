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
  cat(paste("* Populations: ", paste(levels(data$Population), collapse = ", "), "\n", sep = ""))
  cat(paste("* Groups (taxa): ", paste(levels(data$Taxon), collapse = ", "), "\n", sep = ""))
}
