#' @rdname classifda.lda
#' @export
classif.matrix <- function(object, level = "Taxon") {

  checkClass(object, "classifdata")

  #attr(object, "class") <- "data.frame"

  if (level!="Taxon" & level!="Population") stop("Invalid level of grouping. Consider using \"Taxon\" or \"Population\"")

  if (level == "Taxon" )
  {
    classif = table(object$Taxon, object$classif$classification)
    classif = data.frame(unclass(classif))
    colnames(classif) = paste("classif.as:", colnames(classif), sep = "")
    classif = data.frame("Taxon" = attr(classif,"row.names"), "N" = rowSums(classif), classif, row.names = NULL)
    classif = rbind(classif, c("Total", colSums(classif[2:ncol(classif)])))
    NofCorrect = aggregate(object$correct$correct, list(Category=object$Taxon), sum)
    classif = cbind(classif, correct = c( NofCorrect$x, sum(NofCorrect$x)))
    classif = cbind(classif, "percent.correct" = round((classif$correct / as.numeric(classif$N))*100, digits = 2)   )


  } else if (level == "Population")
  {
    classif = table(object$Population, object$classif$classification)
    classif = data.frame(unclass(classif))
    colnames(classif) = paste("classif.as:", colnames(classif), sep = "")
    classif = data.frame("Population" = attr(classif,"row.names"), "N" = rowSums(classif), classif, row.names = NULL)
    tax = data.frame(
      "Population" = object$Population[ ! duplicated(object$Population, object$Taxon)],
          "Taxon" = object$Taxon[ ! duplicated(object$Population, object$Taxon)]
    )
    classif = merge(tax, classif, by = "Population" )
    classif = rbind(classif, c("Total", "", colSums(classif[3:ncol(classif)])))
    NofCorrect = aggregate(object$correct$correct, list(Category=object$Population), sum)
    classif = cbind(classif, correct = c( NofCorrect$x, sum(NofCorrect$x)))
    classif = cbind(classif, "percent.correct" = round((classif$correct / as.numeric(classif$N))*100, digits = 2)   )
  }

  return(classif)
}
