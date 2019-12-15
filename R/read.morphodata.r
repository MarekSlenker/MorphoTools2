read.morphodata<-function(FILE,dec=".",popul=T){
  if (dec==".") xdata<-read.delim(FILE,header=T)
  if (dec==",") xdata<-read.delim2(FILE,header=T)
  if (popul!=T) xdata<-data.frame(xdata[,1],xdata[,2],xdata[,2],xdata[,3:ncol(xdata)])
  names(xdata)[1:3]<-c("ID","Population","Taxon")
  if (!is.factor(xdata$ID)) xdata$ID<-as.factor(xdata$ID)
  if (!is.factor(xdata$Population)) xdata$Population<-as.factor(xdata$Population)
  if (!is.factor(xdata$Taxon)) xdata$Taxon<-as.factor(xdata$Taxon)
  cat("Data strucutre using str(x):","\n")
  str(xdata)
  if (popul==T) cat("\n","List of populations: ","\n",levels(xdata$Population),"\n",fill=T)
  else cat("\n","Population column created from taxa",fill=T)
  cat("List of taxa: ","\n",levels(xdata$Taxon),fill=T)
  return(xdata)}