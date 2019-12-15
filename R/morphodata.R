#' Class "morphodata"
#'
#' @description Objects of this class store morphological data.
#' @usage ## S3 method for class 'morphodata'
#' summary(x)
#' @param data _____.
#' @param x bject of class "morphodata"
#' @return Abject of class "morphodata"
#' @keywords
#' @export
#' @examples
#' read.morphodata("infile.txt")
#' read.morphodata("clipboard")
morphodata <- function(data) {

  names(data)[1:3]<-c("ID","Population","Taxon")
  if (!is.factor(data$ID)) data$ID<-as.factor(data$ID)
  if (!is.factor(data$Population)) data$Population<-as.factor(data$Population)
  if (!is.factor(data$Taxon)) data$Taxon<-as.factor(data$Taxon)


  # class can be set using class() or attr() function
  attr(data, "class") <- "morphodata"
  return(data)


}
