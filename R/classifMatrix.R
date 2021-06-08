#' Format the Classifdata to Summary Table
#' @export
classif.matrix <- function(result, level = "taxon") {

  .checkClass(result, "classifdata")

  if (level!="taxon" & level!="pop" & level!="indiv")  stop("Invalid level of grouping. Consider using \"taxon\", \"pop\" or \"indiv\"")

  if (level == "taxon" )
  {
    classif = table(result$Taxon, result$classif)
    classif = data.frame(unclass(classif))
    colnames(classif) = paste("as.", colnames(classif), sep = "")
    classif = data.frame("Taxon" = attr(classif,"row.names"), "N" = rowSums(classif), classif, row.names = NULL, stringsAsFactors = FALSE)
    classif = rbind(classif, c("Total", colSums(classif[2:ncol(classif)])))

    if (! is.null(result$correct)) {
      NofCorrect = stats::aggregate(result$correct$correct, list(Category=result$Taxon), sum)
      classif = cbind(classif, correct = c( NofCorrect$x, sum(NofCorrect$x)))
      classif = cbind(classif, "correct[%]" = round((classif$correct / as.numeric(classif$N))*100, digits = 2)   )
    }


  } else if (level == "pop")
  {
    classif = table(result$Population, result$classif)
    classif = data.frame(unclass(classif))
    colnames(classif) = paste("as.", colnames(classif), sep = "")
    classif = data.frame("Population" = row.names(classif), "N" = rowSums(classif), classif, row.names = NULL, stringsAsFactors = FALSE)
    tax = data.frame(
      "Population" = result$Population[ ! duplicated(as.character(result$Population), as.character(result$Taxon))],
      "Taxon" = result$Taxon[ ! duplicated(as.character(result$Population), as.character(result$Taxon))],
      stringsAsFactors = FALSE
    )
    classif = merge(tax, classif, by = "Population" )
    classif$Population = as.character(classif$Population)
    classif$Taxon = as.character(classif$Taxon)
    classif = rbind(classif, c("Total", "", colSums(classif[3:ncol(classif)])))

    if (! is.null(result$correct)) {
      NofCorrect = stats::aggregate(result$correct$correct, list(Category=result$Population), sum)
      classif = cbind(classif, correct = c( NofCorrect$x, sum(NofCorrect$x)))
      classif = cbind(classif, "correct[%]" = round((classif$correct / as.numeric(classif$N))*100, digits = 2)   )
    }

  } else if (level == "indiv")
  {
    classif = data.frame("ID" = result$ID, "Population" = result$Population, "Taxon" = result$Taxon,
                         "classification" = result$classif, stringsAsFactors = FALSE)

    if (attr(result, "method" ) == "lda" || attr(result, "method" ) == "qda") {
      colnames(result$prob) =  paste("as.", colnames(result$prob), sep = "")
    }

    classif = cbind(classif,result$prob)

    if (! is.null(result$correct)) {
         classif = cbind(classif, "correct" = result$correct)
    }

    rownames(classif) = NULL

  }

  return(classif)
}
