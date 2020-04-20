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
