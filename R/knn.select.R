#' @rdname classif.lda
#' @export
knn.select<-function(object, crossval="indiv"){

  checkClass(object, "morphodata")

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object' ", call. = FALSE)
  if (crossval!="indiv" & crossval!="pop") stop("Invalid crossvalidation unit. Consider using \"indiv\" or \"pop\"")

  k = as.numeric(1:30)

  object$data = scale(object$data, center = TRUE, scale = TRUE)   #


  ksel = numeric()
  if (crossval=="indiv")
  {
    for (j in 1:10)
    {
      kselj = sapply(k, FUN = function(k){
                                  knn.samp = class::knn.cv(train = object$data,cl =  object$Taxon, k = k, prob = F, use.all = T)
                                  return(sum(as.character(object$Taxon) == as.character(knn.samp))) })
      ksel = rbind(ksel,kselj)

      cat("Tested ", j*10, "% of Ks \n")
    }
  }
  else if (crossval=="pop")
  {
    for (j in 1:10)
    {
      kselj = sapply(k, FUN = function(k){
                                  res = numeric()
                                  for (pop in levels(object$Population)) {
                                    samp = keepByColumn(object, "Population", pop)
                                    train = removeByColumn(object, "Population", pop)

                                    knn.samp = class::knn(train = train$data, test = samp$data, cl = train$Taxon, k = k)

                                    resPop = sum(as.character(samp$Taxon) == as.character(knn.samp))
                                    res<-sum(res,resPop)
                                  }
                                  return(res) })
      ksel = rbind(ksel,kselj)

      cat("Tested ", j*10, "% of Ks \n")
    }
  }

  kselmean = apply(ksel, MARGIN = 2, FUN = mean)
  kselmax = apply(ksel, MARGIN = 2, FUN = max)
  kselmin = apply(ksel, MARGIN = 2, FUN = min)
  plot(kselmean,type="p",pch=16,xlab="K",ylab="correct classifications", ylim=c(min(kselmin),max(kselmax)))

  sapply(k[-1],function(x) graphics::arrows(x, kselmin[x], x, kselmax[x], code = 3, angle = 90, length = 0.07))

  cat("\nThe optimal K is:", which(kselmean==max(kselmean)), "\n")
}










