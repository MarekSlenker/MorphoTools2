#' Replace missing data by population average
#' @export

naMeanSubst <- function(object){
  .checkClass(object, "morphodata")

   meansubst<-function(x){
      m<-mean(x,na.rm=T)
      if (is.nan(m)) m<-NA
      x[which(is.na(x))]<-m
      x = round(x, digits = 3)
      return(x)}

  populs<-levels(object$Population)

  # R passes arguments by value
  for (pop in populs) {
    popPositions = which( object$Population %in% pop)
    object$data[popPositions,] = sapply(object$data[popPositions,], meansubst)

    if (any(is.na(object$data[popPositions,])))
      warning("Unable to replace NAs in characters ", toString(colnames(object$data)[c(is.na(object$data[popPositions[1],]))]),
      " in population ", pop, ". Probably all values of that character are NA.", call. = FALSE)
  }
  return(object)
}



