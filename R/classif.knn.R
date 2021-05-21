#' @rdname classif.lda
#' @export
classif.knn <- function(object, k, crossval = "indiv"){

  .checkClass(object, "morphodata")


  if (any(is.na(object$data))) stop("NA values in 'object'.", call. = FALSE)  # matica musi byt plna
  if (missing(k)) stop("argument \"k\" is missing, with no default.", call. = FALSE)
  if (crossval!="indiv" & crossval!="pop") stop("Invalid crossvalidation unit. Consider using \"indiv\" or \"pop\".")


  ntax<-length(levels(object$Taxon))
  char<-colnames(object$data)

  # kvoli potencial nulovej variabilite v znaku
  # ***************
  # SCALE?????
  # kontrolujem, ci variabilita v ramci znaku je nenulova, inak by to hodil NaN. Ak je nulova, nepouzijem scale.
  object$data = apply(object$data, 2, function(x) (scale(x, center = TRUE, scale = stats::var(x) != 0) ))

  res = .newClassifdata()

  if (crossval=="indiv")
  {
    knn.samp = class::knn.cv(train = object$data, cl = object$Taxon, k = k, prob = T, use.all = T)

    res$ID = as.character(object$ID)
    res$Population = object$Population
    res$Taxon = object$Taxon
    res$classif = knn.samp
    res$prob = round(attr(knn.samp,"prob"), digits = 4)
  }
  else if (crossval=="pop")
  {
    for (i in levels(object$Population)) {
      samp = .keepByColumn(object, "Population", i)
      train = .removeByColumn(object, "Population", i)

      knn.samp = class::knn(train = train$data, test = samp$data, cl = train$Taxon, k = k, prob = TRUE, use.all = TRUE)

      res$ID = c(res$ID, as.character(object$ID[which(i == object$Population)]))
      res$Population = c(res$Population,  as.character(object$Population[object$Population == i]  ))
      res$Taxon = c(res$Taxon, as.character(object$Taxon[object$Population == i] ))

      res$classif = c(res$classif, as.character(knn.samp))
      res$prob = c(res$prob, round(attr(knn.samp,"prob"), digits = 4))


    }

    res$Population = as.factor(res$Population)
    res$Taxon = as.factor(res$Taxon)
    res$classif = as.factor(res$classif)

  }

  res$correct = data.frame("correct" = as.character( res$Taxon) == as.character(res$classif))
  rownames(res$correct) = res$ID

  #res$classif = data.frame("classification" = res$classif)
  #rownames(res$classif) = res$ID

  res$prob = data.frame("Proportion.of.the.votes.for.the.winning.class" = res$prob)
  rownames(res$prob) = res$ID


  attr(res$classif, "prob") = NULL
  attr(res, "method") <- "knn"

  return(res)
}
