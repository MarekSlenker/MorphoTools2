\name{pca.calc}

\alias{pca.calc}


\title{Principal component analysis (PCA)}

\description{
This function performs principal component analysis.
}

\usage{
pca.calc(object)

## S3 method for class 'pcadata'

summary(object)

\code{\link{plot.points}}(pcaResult, axes = c(1,2), xlab = NULL, ylab = NULL,
      pch = 16, col = "black", pt.bg = "white", labels = FALSE,
      legend = FALSE, legend.pos = "topright", ncol = 1, ...)

\code{\link{plot.addLabels.points}}(result, labels = result$objects$ID, include = T,
      axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...)
	  
\code{\link{plot.addLegend}}(result, x = "topright", y = NULL, pch = 16, col = "black",
    pt.bg = "white", pt.cex = cex, pt.lwd = par("lwd"), x.intersp = 1,
    y.intersp = 1, box.type = "o", box.lty = par("lty"), box.lwd = par("lwd"),
    box.col = par("fg"), box.bg = "white", cex = 1, ncol = 1, horiz = FALSE, ...)
	
\code{\link{plot.addEllipses}}(result, axes = c(1,2), probability = 0.95, col = "black",
                  type = "l", lty = 1, lwd = 1, ...)
				  
\code{\link{plot.addSpiders}}(result, axes = c(1,2), col = "black",
                  type = "l", lty = 1, lwd = 1, ...)
	
\code{\link{plot.characters}}(result, axes = c(1,2), xlab , ylab, xlim, ylim, main,
            col = "red", length = 0.1, angle = 15, labels = TRUE, ...)
			
\code{\link{plot.addLabels.characters}}(result, labels = rownames(result$eigenVectors),
    include = T, axes = c(1,2), pos = NULL, offset = 0.5, cex = 0.7, col = NULL, ...)
}

\arguments{
\item{object}{an object of class 'morphodata'.}
}

\value{
object of class '\code{\link{pcadata}}'.
}



\details{


}


\examples{
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

summary(pcaRes)

plot.points(pcaRes, axes = c(1,2), col = c("red","green","blue","red"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=T, legend.pos="bottomright")

}
