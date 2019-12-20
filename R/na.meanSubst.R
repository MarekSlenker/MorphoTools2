#' Replace missing data by population average
#'
#' @description This function substitutes missing data using the average value of the respective character in the
#' respective population.
#'
#' @usage na.meansubst(object)
#'
#' @param object an object of class 'morphodata'.
#'
#' @details Generally, most of the multivariate analyses require a full data matrix.
#' The preferred approach is to reduce the data set to complete observations only (i.e. perform the casewise deletion of
#'  missing data) or to remove characters for which there are missing values.
#'  The use of mean substitution, which introduces values that are not present in the original data, is justified only if
#'   (1) there are relatively few missing values, (2) these missing values are scattered throughout many characters
#'   (each character includes only a few missing values) and (3) removing all individuals or all characters
#'   with missing data would unacceptably reduce the data set.
#'
#' @return object of class 'morphodata'
#'
#' @examples
#' myCompleteDataset = na.meansubst(initialdataset)

#' @export

na.meanSubst <- function(object){
  checkMorphodataClass(object)

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
    object$data[popPositions,] = sapply(object$data[popPositions,],meansubst)
  }
  return(object)
}



