#' @rdname classifSample.lda
#' @export
classifSample.knn <- function(sampleData, trainingData, k){

  checkClass(sampleData, "morphodata")
  checkClass(trainingData, "morphodata")

  # matica musi byt plna
  if (any(is.na(sampleData$data))) stop("NA values in 'sampleData'.", call. = FALSE)
  if (any(is.na(trainingData$data))) stop("NA values in 'trainingData'.", call. = FALSE)


  if (! all(colnames(sampleData$data) == colnames(trainingData$data)))
    stop("Characters of 'sampleData' and 'trainingData' are not the same.", call. = FALSE)

  ntax<-length(levels(trainingData$Taxon))
  char<-colnames(trainingData$data)

  # sampleData$data = scale(sampleData$data, center = TRUE, scale = TRUE)
  # trainingData$data = scale(trainingData$data, center = TRUE, scale = TRUE)

  # kontrolujem, ci variabilita v ramci znaku je nenulova, inak by to hodil NaN
  sampleData$data = apply(sampleData$data, 2, function(x) (scale(x, center = TRUE, scale = var(x) != 0) ))
  trainingData$data = apply(trainingData$data, 2, function(x) (scale(x, center = TRUE, scale = var(x) != 0) ))


  res = newClassifdata()

  knn.samp = class::knn(train = trainingData$data, test = sampleData$data, cl = trainingData$Taxon, k = k, prob = T, use.all = T)

  res$ID = as.character(sampleData$ID)
  res$Population = as.character(sampleData$Population)
  res$Taxon = as.character(sampleData$Taxon)
  res$classif = as.character(knn.samp)
  res$prob = round(attr(knn.samp,"prob"), digits = 4)


  res$classif = as.data.frame(res$classif)
  rownames(res$classif) = res$ID

  res$prob = as.data.frame(res$prob)
  rownames(res$prob) = res$ID
  colnames(res$prob) = "Proportion of the votes for the winning class"

  res$classif = as.data.frame(res$classif)
  rownames(res$classif) = res$ID

  attr(res$classif, "prob") = NULL
  attr(res, "method") <- "knn"


  res$correct = NULL
  attr(res$classif, "prob") = NULL
  attr(res, "method") <- "knn"

  return(res)
}
