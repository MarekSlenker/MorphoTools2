




plot.characters<-function(LOADINGS,axes=c(1,2),col="red",xlab=NULL,ylab=NULL,length=0.1,
                          xlim=c(-1,1),ylim=c(-1,1),labels=TRUE,...) {
  if (is.null(xlab)) xlab<-paste("axis",axes[1])
  if (is.null(ylab)) ylab<-paste("axis",axes[2])
  plot(x=LOADINGS[,1+axes[1]],y=LOADINGS[,1+axes[2]],type="n",xlab=xlab,ylab=ylab,
       xlim=xlim,ylim=ylim,...)
  abline(h=0,v=0,lty=2,col="grey")
  arrows(0,0,LOADINGS[,1+axes[1]],LOADINGS[,1+axes[2]],col=col,length=length,...)
  if (labels==T) text(x=LOADINGS[,1+axes[1]],y=LOADINGS[,1+axes[2]],
                      labels=LOADINGS$Character,cex=0.5,pos=4,offset=0.5)}
