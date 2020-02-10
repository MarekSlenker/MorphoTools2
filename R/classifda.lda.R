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
#'
#' # S3 METHODS
#' SUMMARY
#'
#' PRINT
#'
#' @param object 	an object of class 'morphodata'.
#' @param crossval crossvalidation mode, sets individual ("indiv"; default) or whole populations ("pop") as leave-out unit.
#'
#' @details classif.matrix formats the results of the above three functions as a classification matrix of taxa. For each taxon from the original data, the function shows the number of classifications into all taxa
#' present and the percentage of correct classifications; the total percentage of correct classifications over all taxa is also computed. The results can be exported using the export.res function.
#' matrix<-classif.matrix(results) classif.pmatrix computes the classification matrix for populations; for each population, it shows the numbers of classifications into all taxa present in the data and the percentage of correct
#' classifications. The results can be exported using the export.res function. The detailed classification of populations is very useful, as it can reveal some atypical or incorrectly assigned
#' populations. For example, most of the populations (excluding hybrids) from the sample data are successfully classified using the classif.da function (generally over 70% correct classifications
#'            and often 100%), but two populations (BABL and LES) have only 31% and 15% of correct                                  classifications, respectively; the first one is probably influenced by hybridization, while the latter is
#'                                                                   “atypical” within the sample dataset (it may represent different subspecies). When hybrid populations
#'                                                                                                                         from the sample data are analysed (using classif.samp function), some individuals are usually
#'                                                       classified as C. pseudophrygia and some as C. stenolepis (and none as C. phrygia) within each                                                        population, which clearly demonstrates the intermediacy of these populations.
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

