\name{pca.calc}

\alias{pca.calc}

\title{Principal component analysis (PCA)}

\description{
This function performs Principal Component Analysis.
}

\usage{
pca.calc(object)
}

\arguments{
\item{object}{an object of class 'morphodata'.}
}

\value{
an object of class '\code{\link{pcadata}}'.
}



\details{
The \code{pca.calc} function performs an R type principal component analysis using the R base \code{\link{princomp}} function. Principal component analysis is a variable reduction procedure. It reduces original variables into a smaller number of principal components (artificial variables) that will account for most of the variance in the observed variables.
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

summary(pcaRes)

plotPoints(pcaRes, axes = c(1,2), col = c("red","green","blue","black"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE, legend.pos="bottomright")

}
