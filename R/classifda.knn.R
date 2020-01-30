

#' @rdname classifda.lda
#' @export
classifda.knn <- function(object, k, crossval = "indiv"){

  checkClass(object, "morphodata")

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)
  if (crossval!="indiv" & crossval!="pop") stop("Invalid crossvalidation unit. Consider using \"indiv\" or \"pop\"")


  ntax<-length(levels(object$Taxon))
  char<-colnames(object$data)
  object$data = scale(object$data, center = TRUE, scale = TRUE)   #

  if (crossval=="indiv")
  {
    knn.samp = knn.cv(train = object$data, cl = object$Taxon, k = k, prob = T, use.all = T)
    res = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon, "Classif" = knn.samp, "Prob" = attr(knn.samp,"prob"))
  }
  else if (crossval=="pop")
  {
    pop = levels(object$Population)
    res = data.frame("ID" = factor(), "Population" = factor(), "Taxon" = factor(), "Classif" = factor(), "Prob" = numeric())

    for (i in pop) {
      samp = keepByColumn(object, "Population", i)
      train = removeByColumn(object, "Population", i)

      knn.samp = knn(train = train$data, test = samp$data, cl = train$Taxon, k = k, prob = T, use.all = T)

      resi = data.frame("ID" = samp$ID, "Population" = samp$Population, "Taxon" = samp$Taxon, "Classif" = knn.samp, "Prob" = attr(knn.samp,"prob"))
      res<-rbind(res,resi)
    }
  }
  res$Correct = as.character(res$Taxon) == as.character(res$Classif)
  row.names(res) = NULL
  attr(res, "class") <- "classifdata"
  return(res)
}
