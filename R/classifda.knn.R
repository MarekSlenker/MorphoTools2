#' @rdname classifda.lda
#' @export
classifda.knn <- function(object, k, crossval = "indiv"){

  checkClass(object, "morphodata")


  if (any(is.na(object$data))) stop("NA values in \"object\" ", call. = FALSE)  # matica musi byt plna
  if (missing(k)) stop("argument \"k\" is missing, with no default", call. = FALSE)
  if (crossval!="indiv" & crossval!="pop") stop("Invalid crossvalidation unit. Consider using \"indiv\" or \"pop\"")


  ntax<-length(levels(object$Taxon))
  char<-colnames(object$data)
  object$data = scale(object$data, center = TRUE, scale = TRUE)   #

  res = newClassifdata()

  if (crossval=="indiv")
  {
    knn.samp = class::knn.cv(train = object$data, cl = object$Taxon, k = k, prob = T, use.all = T)

    res$ID = object$ID
    res$Population = object$Population
    res$Taxon = object$Taxon
    res$classif = knn.samp
    res$prob = attr(knn.samp,"prob")
  }
  else if (crossval=="pop")
  {
    for (i in levels(object$Population)) {
      samp = keepByColumn(object, "Population", i)
      train = removeByColumn(object, "Population", i)

      knn.samp = class::knn(train = train$data, test = samp$data, cl = train$Taxon, k = k, prob = TRUE, use.all = TRUE)

      res$ID = c(res$ID, as.character(object$ID[which(i == object$Population)]))
      res$Population = c(res$Population,  as.character(object$Population[which(i == object$Population)]))
      res$Taxon = c(res$Taxon, as.character(object$Taxon[which(i == object$Population)]))
      res$classif = c(res$classif, as.character(knn.samp))
      res$prob = rbind(res$prob, round(attr(knn.samp,"prob"), digits = 4))
    }
  }

  res$correct = as.character(res$Taxon) == as.character(res$classif)
  attr(res$classif, "prob") = NULL
  attr(res, "method") <- "knn"

  return(res)
}
