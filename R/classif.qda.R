#' Classificatory discriminant analysis
#' @export
classif.qda <- function(object, crossval="indiv") {

  .checkClass(object, "morphodata")

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object'.", call. = FALSE)
  if (crossval!="indiv" & crossval!="pop") stop("Invalid crossvalidation unit. Consider using \"indiv\" or \"pop\".")


  ntax<-length(levels(object$Taxon))


  res = .newClassifdata()

  if (crossval=="indiv")
  {
    #qda.res = MASS::qda(stats::as.formula(paste("object$Taxon ~ ", paste(colnames(object$data), collapse="+"))), data=object$data, CV=TRUE, prior = rep(1/ntax,ntax))
	qda.res = MASS::qda(object$Taxon ~ . , data=object$data, CV=TRUE, prior = rep(1/ntax,ntax))

    res$ID = as.character(object$ID)
    res$Population = object$Population
    res$Taxon = object$Taxon
    res$classif = qda.res$class
    res$prob = round(qda.res$posterior, digits = 4)

  }
  else if (crossval=="pop")
  {

    for (i in levels(object$Population)) {
      samp = .keepByColumn(object, "Population", i)
      train = .removeByColumn(object, "Population", i)

      #qda.train = MASS::qda(stats::as.formula(paste("train$Taxon ~ ", paste(colnames(object$data), collapse = "+"))), data = train$data, prior = rep(1/ntax,ntax))
      qda.train = MASS::qda(train$Taxon ~ . , data = train$data, prior = rep(1/ntax,ntax))
	    qda.samp = stats::predict(qda.train, samp$data)

      res$ID = c(res$ID, as.character(object$ID[which(i == object$Population)]))
      res$Population = c(res$Population,  as.character(object$Population[object$Population == i]  ))
      res$Taxon = c(res$Taxon,                 as.character(object$Taxon[object$Population == i] ))

      res$classif = c(res$classif, as.character(qda.samp$class))
      res$prob = rbind(res$prob, round(qda.samp$posterior, digits = 4))
    }

    res$Population = as.factor(res$Population)
    res$Taxon = as.factor(res$Taxon)
    res$classif = as.factor(res$classif)

  }

  res$correct = data.frame("correct" = as.character( res$Taxon) == as.character(res$classif))
  rownames(res$correct) = res$ID

  #res$classif = data.frame("classification" = res$classif)
  #rownames(res$classif) = res$ID

  res$prob = as.data.frame(res$prob)

  attr(res, "method") <- "qda"

  return(res)
}

