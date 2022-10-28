#' Classificatory discriminant analysis
#' @export
classif.lda <- function(object, crossval="indiv") {

  # .checkClass(object, "morphodata")
  if (!inherits(object, "morphodata")) stop("object not of class \"morphodata\"")

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object'.", call. = FALSE)
  if (crossval!="indiv" & crossval!="pop") stop("Invalid crossvalidation unit. Consider using \"indiv\" or \"pop\".", call. = FALSE)


  ntax<-length(levels(object$Taxon))
  char<-colnames(object$data)

  res = .newClassifdata()

  if (crossval=="indiv")
  {
    lda.res = MASS::lda(object$Taxon ~ . , data=object$data, CV=TRUE, prior = rep(1/ntax,ntax))

    lda.res$means = .calcMeans(object)


    # lda.res$class
    # class = array(NA, dim = c(s, ntax)) ## group means
    # for(i in 1:s) {
    #   for (j in 1:ntax) {
    #     class[i,j] = class.funs[1,j]+sum(x[i,]*class.funs[-1,j])
    #   }
    # }
    # lda.res$class = factor(max.col(class), levels = lda.res$lev)

    res$classif.funs = .classFuns(object)

    res$ID = as.character(object$ID)
    res$Population = object$Population
    res$Taxon = object$Taxon
    res$classif = lda.res$class
    res$prob = round(lda.res$posterior, digits = 4)

  }
  else if (crossval=="pop")
  {
    #levels(res$Population) = levels(object$Population)


    # SKUSKA
    xx =  array(NA, dim = c(ncol(object$data)+1, ntax, length(levels(object$Population))),
               dimnames = list(c("constant", colnames(object$data)), levels(object$Taxon), levels(object$Population)))


    for (i in levels(object$Population)) {
      samp = .keepByColumn(object, "Population", i)
      train = .removeByColumn(object, "Population", i)

      #lda.train = MASS::lda(stats::as.formula(paste("train$Taxon ~ ", paste(char, collapse = "+"))), data = train$data, prior = rep(1/ntax,ntax))
      lda.train = MASS::lda(train$Taxon ~ . , data = train$data, prior = rep(1/ntax,ntax))

      # SKUSKA
      cf = .classFuns(train)
      for (col in colnames(cf)  ) {
       xx[,col,i] = cf[,col]
      }

      lda.samp = stats::predict(lda.train, samp$data)

      res$ID = c(res$ID, as.character(object$ID[which(i == object$Population)]))
      res$Population = c(res$Population,  as.character(object$Population[object$Population == i]  ))
      res$Taxon = c(res$Taxon,                 as.character(object$Taxon[object$Population == i] ))
      res$classif = c(res$classif, as.character(lda.samp$class))
      res$prob = rbind(res$prob, round(lda.samp$posterior, digits = 4))
    }

    res$Population = as.factor(res$Population)
    res$Taxon = as.factor(res$Taxon)
    res$classif = as.factor(res$classif)
    res$classif.funs = apply(xx,c(1,2),mean)   # SKUSKA
  }


  res$correct = data.frame("correct" = as.character( res$Taxon) == as.character(res$classif))
  rownames(res$correct) = res$ID

  #res$classif = data.frame("classification" = res$classif)
  #rownames(res$classif) = res$ID

  res$prob = as.data.frame(res$prob)

  attr(res, "method") <- "lda"

  return(res)
}

.classFuns <- function(object) {
  ntax<-length(levels(object$Taxon))
  v <- ncol(object$data) ## variables
  s <- nrow(object$data) ## variables
  char<-colnames(object$data)

  m = .calcMeans(object)

  w <- array(NA, dim = c(v, v, ntax), dimnames = list(colnames(object$data), colnames(object$data), levels(object$Taxon)))

  for(i in levels(object$Taxon)){
    tmp <- scale(subset(object$data, object$Taxon == i), scale = FALSE)
    w[,,i] <- t(tmp) %*% tmp
  }

  W <- w[,,1]
  for(i in 2:ntax)
    W <- W + w[,,i]

  V <- W/(nrow(object$data) - ntax)
  iV <- solve(V, tol = -Inf) # inac to blbne

  class.funs <- matrix(NA, nrow = v + 1, ncol = ntax)
  colnames(class.funs) <- levels(object$Taxon)
  rownames(class.funs) <- c("constant", colnames(object$data))

  for(i in 1:ntax) {
    class.funs[1, i] <- -0.5 * t(m[i,]) %*% iV %*% (m[i,])
    class.funs[2:(v+1) ,i] <- iV %*% (m[i,])
  }
  return(class.funs)
}

.calcMeans <- function(object) {
  ntax<-length(levels(object$Taxon))
  v <- ncol(object$data) ## variables
  char<-colnames(object$data)

  xm = array(NA, dim = c(ntax, v), dimnames = list(levels(object$Taxon), char)) ## group means
  for(i in levels(object$Taxon)){
    xm[i,] = apply(object$data[object$Taxon == i, ], 2, mean)
  }
  return(xm)
}

