#' @rdname classifSample.lda
#' @export
classifSample.knn <- function(sampleData, trainingData, k){

  .checkClass(sampleData, "morphodata")
  .checkClass(trainingData, "morphodata")

  # matica musi byt plna
  if (any(is.na(sampleData$data))) stop("NA values in 'sampleData'.", call. = FALSE)
  if (any(is.na(trainingData$data))) stop("NA values in 'trainingData'.", call. = FALSE)

  if (missing(k)) stop("argument \"k\" is missing, with no default.", call. = FALSE)

  if (! all(colnames(sampleData$data) == colnames(trainingData$data)))
    stop("characters of 'sampleData' and 'trainingData' are not the same.", call. = FALSE)

  ntax<-length(levels(trainingData$Taxon))
  char<-colnames(trainingData$data)



  # ---------
  # MANUAL SCALE
  trainingData$data = as.matrix(trainingData$data)
  # center
  center = colMeans(trainingData$data, na.rm = TRUE)
  trainingData$data = sweep(trainingData$data, 2, center, check.margin = FALSE)
  # scale
  f <- function(x) {
    x <- x[!is.na(x)]
    sqrt(sum(x^2)/max(1, length(x) - 1))
  }
  scale = apply(trainingData$data, 2, f)
  scale[which(scale == 0)] = 1 # nemozme delit nulou
  trainingData$data <- sweep(trainingData$data, 2, scale, "/", check.margin = FALSE)

  sampleData$data = as.matrix(sampleData$data)
  sampleData$data = sweep(sampleData$data, 2, center, check.margin = FALSE)
  sampleData$data = sweep(sampleData$data, 2, scale, "/", check.margin = FALSE)
  # ---------

  # kontrolujem, ci variabilita v ramci znaku je nenulova, inak by to hodil NaN
  # SCALOVAT?????
  # sampleData$data = apply(sampleData$data, 2, function(x) (scale(x, center = TRUE, scale = stats::var(x) != 0) ))
  # trainingData$data = apply(trainingData$data, 2, function(x) (scale(x, center = TRUE, scale = stats::var(x) != 0) ))


  res = .newClassifdata()

  knn.samp = class::knn(train = trainingData$data, test = sampleData$data, cl = trainingData$Taxon, k = k, prob = TRUE, use.all = TRUE)

  #res$ID = as.character(sampleData$ID)
  #res$Population = as.character(sampleData$Population)
  #res$Taxon = as.character(sampleData$Taxon)
  ##res$classif = as.character(knn.samp)
  #res$prob = round(attr(knn.samp,"prob"), digits = 4)

  res$ID = as.character(sampleData$ID)
  res$Population = sampleData$Population
  res$Taxon = sampleData$Taxon
  res$classif = knn.samp
  res$prob = round(attr(knn.samp,"prob"), digits = 4)


  #res$classif = data.frame("classification" = res$classif)
  #rownames(res$classif) = res$ID

  res$prob = data.frame("Proportion.of.the.votes.for.the.winning.class" = res$prob)
  rownames(res$prob) = res$ID


  res$correct = NULL
  #attr(res$classif, "prob") = NULL
  attr(res, "method") <- "knn"

  return(res)
}
