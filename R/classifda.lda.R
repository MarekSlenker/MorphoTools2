#' Classificatory discriminant analysis
#'
#' @description These functions computes discriminant function for classifying observations. Linear discriminant function or nonparametric k-nearest-neighbor method can be used.
#'
#' @usage
#' classifda.lda(object, crossval = "indiv")
#'
#' knn.select(object, crossval = "indiv")
#' classifda.knn(object, k, crossval = "indiv")
#'
#' @param object 	an object of class 'morphodata'.
#' @param crossval crossvalidation mode, sets individual ("indiv"; default) or whole populations ("pop") as leave-out unit.
#'
#' @details dsa vrati objekt typu "classifdata
#'
#' @examples
#' classifda.lda(morphodata, crossval = "indiv")
#' classifda.lda(morphodata, crossval = "pop")
#' classifda.knn(morphodata, k, crossval = "indiv")
#'
#' @export
classifda.lda <- function(object, crossval="indiv") {

  checkClass(object, "morphodata")

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)
  if (crossval!="indiv" & crossval!="pop") stop("Invalid crossvalidation unit. Consider using \"indiv\" or \"pop\"")


  ntax<-length(levels(object$Taxon))
  char<-colnames(object$data)

  if (crossval=="indiv")
  {
    lda.res = lda(as.formula(paste("object$Taxon ~ ", paste(char, collapse="+"))), data=object$data,  CV=TRUE, prior = rep(1/ntax,ntax))
    res = data.frame("ID" = object$ID, "Population" = object$Population, "Taxon" = object$Taxon, "Classif" = lda.res$class, round(lda.res$posterior, digits = 6))
  }
  else if (crossval=="pop")
  {
    pop = levels(object$Population)
    res = data.frame(replicate(4,factor(),simplify=F), replicate(ntax,numeric(),simplify=F))
    names(res) = c("ID", "Population", "Taxon", "Classif", levels(object$Taxon))

    for (i in pop) {
      #samp = object$data[object$Population == i,]
      #train = object$data[object$Population != i,]

      samp = keepByColumn(object, "Population", i)
      train = removeByColumn(object, "Population", i)

      lda.train = lda(as.formula(paste("train$Taxon ~ ", paste(char, collapse = "+"))), data = train$data, prior = rep(1/ntax,ntax))
      lda.samp = predict(lda.train, samp$data)
      resi = data.frame("ID"=samp$ID, "Population" = samp$Population, "Taxon" = samp$Taxon, "Classif" = lda.samp$class, round(lda.samp$posterior, digits = 6))
      res<-rbind(res,resi)
    }

  }

  res$Correct = as.character(res$Taxon) == as.character(res$Classif)
  row.names(res) = NULL

  attr(res, "class") <- "classifdata"

  return(res)
}

