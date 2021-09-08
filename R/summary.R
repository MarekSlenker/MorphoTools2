#' @name summary
#' @title Object Summaries
#' @export
summary.morphodata <- function(object, ...) {

  # cat summary
  cat("Object of class 'morphodata'\n")
  cat(paste(" - contains ", length(levels(object$Population)), " populations\n", sep = ""))
  cat(paste(" - contains ", length(levels(object$Taxon)), " taxa (defined groups)\n", sep = ""))
  cat("\n")
  cat(paste("Populations: ", paste(levels(object$Population), collapse = ", "), "\n", sep = ""))
  cat(paste("Taxa (defined groups): ", paste(levels(object$Taxon), collapse = ", "), "\n", sep = ""))
}



#' @rdname summary
#' @export
summary.pcadata <- function(object, ...) {

  cat("Object of class 'pcadata'; storing results of principal component analysis\n")
  cat("\nVariation explained by individual axes")
  if (object$rank>4) {
    cat(" (listing of axes is truncated):\n")
  } else {
    cat(":\n")
  }

  descrTable = data.frame(row.names = names(object$eigenvaluesAsPercentages[1: min(object$rank, 4)]),
                          "Eigenvalues" = round(object$eigenvalues[1: min(object$rank, 4)], digits = 4),
                          "Eigenvalues as percentages" = round(object$eigenvaluesAsPercentages[1: min(object$rank, 4)], digits = 4),
                          "Cumulative percentage of eigenvalues" = round(object$cumulativePercentageOfEigenvalues[1: min(object$rank, 4)], digits = 4)
                          )
  names(descrTable) = gsub(pattern = '\\.' , replacement = " ", x = names(descrTable))
  descrTable = t(descrTable)

  print(descrTable)

  cat("\nEigenvectors")
  if (object$rank>4) {
    cat(" (listing of axes is truncated):\n")
  } else {
    cat(":\n")
  }

  print(object$eigenvectors[,1:min(object$rank, 4)])

}


#' @rdname summary
#' @export
summary.pcoadata <- function(object, ...) {

  cat("Object of class 'pcoadata'; storing results of principal coordinates analysis\n")
  cat("Resemblance coefficient: ", object$distMethod,"\n")
  cat("\nVariation explained by individual axes")
  if (object$rank>4) {
    cat(" (listing of axes is truncated):\n")
  } else {
    cat(":\n")
  }

  descrTable = data.frame(row.names = names(object$eigenvaluesAsPercentages[1: min(object$rank, 4)]),
                          "Eigenvalues" = round(object$eigenvalues[1: min(object$rank, 4)], digits = 4),
                          "Eigenvalues as percentages" = round(object$eigenvaluesAsPercentages[1: min(object$rank, 4)], digits = 4),
                          "Cumulative percentage of eigenvalues" = round(object$cumulativePercentageOfEigenvalues[1: min(object$rank, 4)], digits = 4)
  )
  names(descrTable) = gsub(pattern = '\\.' , replacement = " ", x = names(descrTable))
  descrTable = t(descrTable)

  print(descrTable)
}


#' @rdname summary
#' @export
summary.nmdsdata <- function(object, ...) {

  cat("Object of class 'nmdsdata'; storing results of non-metric multidimensional scaling\n")
  cat("Resemblance coefficient: ", object$distMethod,"\n")

  cat("\nDimensions: ", object$rank)
  cat("\nStress: ", object$stress)
  cat("\nScores scaled to unit root mean square, rotated to principal components")

}


#' @rdname summary
#' @export
summary.cdadata <- function(object, ...) {

  cat("Object of class 'cdadata'; storing results of canonical discriminant analysis\n")
  cat("\nVariation explained by individual axes")
  if (object$rank>4) {
    cat(" (listing of axes is truncated):\n")
  } else {
    cat(":\n")
  }

  descrTable = data.frame(row.names = colnames(object$objects$scores[,1: min(object$rank, 4)]),
                          "Eigenvalues" = round(object$eigenvalues[1: min(object$rank, 4)], digits = 4),
                          "Eigenvalues as percentages" = round(object$eigenvaluesAsPercentages[1: min(object$rank, 4)], digits = 4),
                          "Cumulative percentage of eigenvalues" = round(object$cumulativePercentageOfEigenvalues[1: min(object$rank, 4)], digits = 4)
  )
  names(descrTable) = gsub(pattern = '\\.' , replacement = " ", x = names(descrTable))
  descrTable = t(descrTable)

  print(descrTable)

  cat("\nTotal canonical structure coefficients")
  if (object$rank>4) {
    cat(" (listing of axes is truncated):\n")
  } else {
    cat(":\n")
  }

  print(object$totalCanonicalStructure[,1: min(object$rank, 4)])

}


#' @rdname summary
#' @export
summary.classifdata <- function(object, ...) {

  cat("Object of class 'classifdata'; storing results of classificatory discriminant analysis\n\n")

  descrTable = data.frame("ID" = object$ID,
                          "Population" = object$Population,
                          "Taxon" = object$Taxon,
                          "classification" = object$classif,
                          "probability" = object$prob,
                          "correct" = object$correct)
  rownames(descrTable) = NULL

  print(descrTable)
}


