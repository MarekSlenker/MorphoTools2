#' @rdname classif.lda
#' @export
classif.knn <- function(object, k, crossval = "indiv"){

  checkClass(object, "morphodata")


  if (any(is.na(object$data))) stop("NA values in 'object'.", call. = FALSE)  # matica musi byt plna
  if (missing(k)) stop("argument \"k\" is missing, with no default.", call. = FALSE)
  if (crossval!="indiv" & crossval!="pop") stop("Invalid crossvalidation unit. Consider using \"indiv\" or \"pop\".")


  ntax<-length(levels(object$Taxon))
  char<-colnames(object$data)
  # object$data = scale(object$data, center = TRUE, scale = TRUE)

  res = newClassifdata()

  if (crossval=="indiv")
  {
    object$data = scale(object$data, center = TRUE, scale = TRUE)

    knn.samp = class::knn.cv(train = object$data, cl = object$Taxon, k = k, prob = T, use.all = T)

    res$ID = as.character(object$ID)
    res$Population = as.character(object$Population)
    res$Taxon = as.character(object$Taxon)
    res$classif = as.character(knn.samp)
    res$prob = attr(knn.samp,"prob")
  }
  else if (crossval=="pop")
  {
    for (i in levels(object$Population)) {
      samp = keepByColumn(object, "Population", i)
      train = removeByColumn(object, "Population", i)

      # samp$data = scale(samp$data, center = TRUE, scale = TRUE)
      # train$data = scale(train$data, center = TRUE, scale = TRUE)

      # kontrolujem, ci variabilita v ramci znaku je nenulova, inak by to hodil NaN. Ak je nulova, nepouzijem scale.
      samp$data = apply(samp$data, 2, function(x) (scale(x, center = TRUE, scale = stats::var(x) != 0) ))
      train$data = apply(train$data, 2, function(x) (scale(x, center = TRUE, scale = stats::var(x) != 0) ))

      knn.samp = class::knn(train = train$data, test = samp$data, cl = train$Taxon, k = k, prob = TRUE, use.all = TRUE)
      res$ID = c(res$ID, as.character(object$ID[which(i == object$Population)]))
      res$Population = c(res$Population,  as.character(object$Population[which(i == object$Population)]))
      res$Taxon = c(res$Taxon, as.character(object$Taxon[which(i == object$Population)]))
      res$classif = c(res$classif, as.character(knn.samp))
      res$prob = c(res$prob, round(attr(knn.samp,"prob"), digits = 4))
    }
  }

  res$correct = data.frame("correct" = as.character( res$Taxon) == as.character(res$classif))
  rownames(res$correct) = res$ID

  res$classif = data.frame("classification" = res$classif)
  rownames(res$classif) = res$ID

  res$prob = data.frame("Proportion of the votes for the winning class" = res$prob)
  rownames(res$prob) = res$ID


  attr(res$classif, "prob") = NULL
  attr(res, "method") <- "knn"

  return(res)
}
