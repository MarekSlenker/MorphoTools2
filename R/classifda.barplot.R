


# @rdname classifda.lda
# @export
classifda.barplot<-function(object, tax.ord = NULL, poplabels = NULL, col = NULL, col.lines = NULL, cex.axis = 1, cex.lab = 1, cex.pop = 1, ylab="posterior probability", tcl = -0.5, ref.line = FALSE, ref.pars = list())
{
  checkClass(object, "classifdata")




  if (is.null(tax.ord)) {
    ordr = order(object$Taxon,object$Population)


  }
    xdata<-CLASSIF[order(CLASSIF$Taxon,CLASSIF$Population),]
  else {xdata<-CLASSIF[order(match(CLASSIF$Taxon,tax.ord),CLASSIF$Population),]


  xdata<-xdata[,c("ID","Population","Taxon","Classif",tax.ord,"Correct")]}
  if (is.null(poplabels)) poplist<-unique(xdata$Population) else poplist<-poplabels
  nind<-as.data.frame(table(xdata$Population))
  nind<-nind[order(match(nind$Var1,poplist)),]
  nind<-nind$Freq
  linepos<-c(0,cumsum(nind))
  if (is.null(col.lines)) col.lines<-par("fg")
  barplot(as.matrix(t(xdata[,5:(ncol(xdata)-1)])),space=0,border=NA,cex.axis=cex.axis,
          cex.lab=cex.lab,col=col,ylab=ylab,width=1,xlim=c(0,sum(nind)),tcl=tcl,
          axisnames=F)
  for (i in 1:length(linepos)) abline (v=linepos[i],col=col.lines)
  textpos<-linepos[-length(linepos)]+nind/2
  mtext(poplist,at=textpos,side=1,las=2,cex=cex.pop,line=0.2)
  if (is.null(ref.pars$y)) ref.pars$y<-0.5
  if (is.null(ref.pars$col)) ref.pars$col<-par("fg")
  if (is.null(ref.pars$lwd)) ref.pars$lwd<-par("lwd")
  if (is.null(ref.pars$lty)) ref.pars$lty<-2
  if (ref.line==TRUE) lines(x=c(0,sum(nind)),y=c(ref.pars$y,ref.pars$y),col=ref.pars$col,
                            lwd=ref.pars$lwd,lty=ref.pars$lty)}
