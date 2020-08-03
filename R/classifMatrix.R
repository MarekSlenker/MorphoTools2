

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


    POKRACUJ:



    percentCorrect = classif$correct / classif$N

    with(classif,(correct/N)*100)

    last = classif[1,]
    last[1] = "Total"
    last[2:length(last)] = colSums(classif[2:ncol(classif)])
    classif = rbind(classif, last)

    names(classif)[ncol(classif)] = "percent.correct"
    classif$percent.correct = with(classif,(percent.correct/N)*100)


    Taxon  ph  ps st   N percent.correct
    1    ph 119   6  0 125        95.20000
    2    ps   5 159  5 169        94.08284
    3    st   0   9 65  74        87.83784
    4 Total 124 174 70 368        93.20652

  } else if (level == "Population")
  {
    classif = table(object$Population, object$Classif)

    classif = data.frame(unclass(classif))

    classif = data.frame(Population=attr(classif,"row.names"),classif,row.names=NULL)

    tax = unique(object[,c(2,3)])
    classif = merge(tax, classif)
    classif$N = rowSums(classif[3:ncol(classif)])
    ncor = aggregate(Correct ~ Population, data = object, sum)
    classif = merge(classif, ncor)
    names(classif)[ncol(classif)]<-"percent.correct"
    classif$percent.correct<-with(classif,(percent.correct/N)*100)
  }

  return(classif)
}
