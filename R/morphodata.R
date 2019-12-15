

morphodata <- function(data) {

  names(data)[1:3]<-c("ID","Population","Taxon")
  if (!is.factor(data$ID)) data$ID<-as.factor(data$ID)
  if (!is.factor(data$Population)) data$Population<-as.factor(data$Population)
  if (!is.factor(data$Taxon)) data$Taxon<-as.factor(data$Taxon)


  # class can be set using class() or attr() function
  attr(data, "class") <- "morphodata"
  return(data)


}
