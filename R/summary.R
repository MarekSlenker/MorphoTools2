#' @name summary
#' @title Object Summaries
#' @export
summary.morphodata <- function(object, ...) {
  #  is of morphodata class?  (keby tuto genericku metodu chel pouzit niekto priamo)
  checkClass(object, "morphodata")

  # cat summary

  cat("object of class 'morphodata'\n")
  cat(paste(" - contains ", length(levels(object$Population)), " populations\n", sep = ""))
  cat(paste(" - contains ", length(levels(object$Taxon)), " defined groups (taxa)\n", sep = ""))
  cat("\n")
  cat(paste("Populations: ", paste(levels(object$Population), collapse = ", "), "\n", sep = ""))
  cat(paste("Groups (taxa): ", paste(levels(object$Taxon), collapse = ", "), "\n", sep = ""))
}



#' @rdname summary
#' @export
summary.pcadata <- function(object, ...) {
  checkClass(object, "pcadata")

  cat("object of class 'pcadata'; storing original data and results of Principal Component Analysis\n")
  cat("\nVariation explained by individual axes:\n")

  descrTable = data.frame(row.names = names(object$eigenvaluesAsPercent),
                          "Eigenvalues" = round(object$eigenValues, digits = 4),
                          "Eigenvalues as Percent" = round(object$eigenvaluesAsPercent, digits = 4),
                          "Cumulative Percentage of Eigenvalues" = round(object$cumulativePercentageOfEigenvalues, digits = 4)
                          )
  names(descrTable) = gsub(pattern = '\\.' , replacement = " ", x = names(descrTable))
  descrTable = t(descrTable)

  print(descrTable)

  cat("\nEigenvectors:\n")

  print(object$eigenVectors[,1:4])

}


#' @rdname summary
#' @export
summary.cdadata <- function(object, ...) {
  checkClass(object, "cdadata")

  cat("object of class 'cdadata'; storing original data and results of Canonical Discriminant Analysis\n")



  descrTable = data.frame(row.names = c(1: object$rank),
                          "Eigenvalues" = round(object$eigenValues[1:object$rank], digits = 4),
                          "Eigenvalues as Percent" = round(object$eigenvaluesAsPercent[1:object$rank], digits = 4),
                          "Cumulative Percentage of Eigenvalues" = round(object$cumulativePercentageOfEigenvalues[1:object$rank], digits = 4)
  )
  names(descrTable) = gsub(pattern = '\\.' , replacement = " ", x = names(descrTable))


  cat("\nVariation explained by individual axes:\n")
  print(descrTable)


  cat("\nTotal canonical structure coefficients:\n")

  print(object$totalCanonicalStructure[,1:object$rank])

}


#' @rdname summary
#' @export
summary.classifdata <- function(object, ...) {
  checkClass(object, "classifdata")

  cat("object of class 'classifdata'; storing results of Classificatory Discriminant Analysis\n\n")

  descrTable = data.frame("ID" = object$ID,
                          "Population" = object$Population,
                          "Taxon" = object$Taxon,
                          "classification" = object$classif,
                          "probability" = object$prob,
                          "correct" = object$correct)
  rownames(descrTable) = NULL

    print(descrTable)
}









