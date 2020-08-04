#' @rdname classif.lda
#' @export
classif.matrix <- function(object, level = "taxon") {

  checkClass(object, "classifdata")

  if (level!="taxon" & level!="pop" & level!="indiv")  stop("Invalid level of grouping. Consider using \"taxon\", \"pop\" or \"indiv\"")

  if (level == "taxon" )
  {
    classif = table(object$Taxon, object$classif$classification)
    classif = data.frame(unclass(classif))
    colnames(classif) = paste("as.", colnames(classif), sep = "")
    classif = data.frame("Taxon" = attr(classif,"row.names"), "N" = rowSums(classif), classif, row.names = NULL)
    classif = rbind(classif, c("Total", colSums(classif[2:ncol(classif)])))

  if (! is.null(object$correct)) {
    NofCorrect = stats::aggregate(object$correct$correct, list(Category=object$Taxon), sum)
    classif = cbind(classif, correct = c( NofCorrect$x, sum(NofCorrect$x)))
    classif = cbind(classif, "correct[%]" = round((classif$correct / as.numeric(classif$N))*100, digits = 2)   )
  }


  } else if (level == "pop")
  {
    classif = table(object$Population, object$classif$classification)
    classif = data.frame(unclass(classif))
    colnames(classif) = paste("as.", colnames(classif), sep = "")
    classif = data.frame("Population" = attr(classif,"row.names"), "N" = rowSums(classif), classif, row.names = NULL)
    tax = data.frame(
      "Population" = object$Population[ ! duplicated(object$Population, object$Taxon)],
          "Taxon" = object$Taxon[ ! duplicated(object$Population, object$Taxon)]
    )
    classif = merge(tax, classif, by = "Population" )
    classif = rbind(classif, c("Total", "", colSums(classif[3:ncol(classif)])))

    if (! is.null(object$correct)) {
      NofCorrect = stats::aggregate(object$correct$correct, list(Category=object$Population), sum)
      classif = cbind(classif, correct = c( NofCorrect$x, sum(NofCorrect$x)))
      classif = cbind(classif, "correct[%]" = round((classif$correct / as.numeric(classif$N))*100, digits = 2)   )
    }

  } else if (level == "indiv")
  {
    classif = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon,
                         "classification" = object$classif)

    if (attr(object, "method" ) == "lda") {
      colnames(object$prob) =  paste("probability.of:", colnames(object$prob), sep = "")
    }

    classif = cbind(classif,object$prob)

    if (! is.null(object$correct)) {
         classif = cbind(classif, "correct" = object$correct)
    }

    rownames(classif) = NULL

  }

  return(classif)
}
