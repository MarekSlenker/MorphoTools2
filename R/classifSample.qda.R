#' Classificatory discriminant analysis
#' @export
classifSample.qda <- function(sampleData, trainingData) {

  .checkClass(sampleData, "morphodata")
  .checkClass(trainingData, "morphodata")

  # matica musi byt plna
  if (any(is.na(sampleData$data))) stop("NA values in 'sampleData'.", call. = FALSE)
  if (any(is.na(trainingData$data))) stop("NA values in 'trainingData'.", call. = FALSE)

  if (! all(colnames(sampleData$data) == colnames(trainingData$data)))
    stop("Characters of 'sampleData' and 'trainingData' are not the same.", call. = FALSE)


  ntax<-length(levels(trainingData$Taxon))
  char<-colnames(trainingData$data)


  res = .newClassifdata()

  qda.train = MASS::qda(stats::as.formula(paste("trainingData$Taxon ~ ", paste(char, collapse="+"))),
                        data=trainingData$data, prior = rep(1/ntax,ntax))

  qda.samp = stats::predict(qda.train,sampleData$data)


  res$ID = as.character(sampleData$ID)
  res$Population = as.character(sampleData$Population)
  res$Taxon = as.character(sampleData$Taxon)

  res$classif = data.frame("classification" = as.character(qda.samp$class))
  rownames(res$classif) = res$ID

  res$prob = round(qda.samp$posterior, digits = 4)
  res$prob = as.data.frame(res$prob)


  res$correct = NULL
  attr(res, "method") <- "qda"

  return(res)
}

