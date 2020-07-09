\name{pca.calc}

\alias{pca.calc}

\title{Principal component analysis (PCA)}

\description{
This function performs principal component analysis.
}

\usage{
pca.calc(object)
}

\arguments{
\item{object}{an object of class 'morphodata'.}
}

\value{
object of class '\code{\link{pcadata}}'.
}



\details{
Principal components analysis, wrapper over \code{\link{prcomp}} method.
}

\examples{
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

summary(pcaRes)

plot.points(pcaRes, axes = c(1,2), col = c("red","green","blue","black"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE, legend.pos="bottomright")

}
