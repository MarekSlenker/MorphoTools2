\name{pca.calc}
\alias{pca.calc}

\title{Principal Component Analysis}

\description{
This function performs principal component analysis.
}

\usage{
pca.calc(object)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
}

\value{
an object of class \code{\link{pcadata}} with the following elements:

\item{objects}{}
\tabular{lrl}{
  \tab \code{ID} \tab IDs of each row of \code{scores} object. \cr
  \tab \code{Population} \tab population membership of each row of \code{scores} object. \cr
  \tab \code{Taxon} \tab taxon membership of each row of \code{scores} object. \cr
  \tab \code{scores} \tab ordination scores of cases (objects, OTUs). \cr
}
\item{eigenVectors}{matrix of eigenvectors (i.e., a matrix of characters loadings).}
\item{eigenValues}{eigenvalues of principal components, i.e., proportion of variation of the original dataset expressed by individual axes.}
\item{eigenvaluesAsPercent}{eigenvalues as percent, percentage of their total sum. }
\item{cumulativePercentageOfEigenvalues}{cumulative percentage of eigenvalues. }
\item{groupMeans}{\code{data.frame} containing the means for the taxa.}
\item{rank}{number of principal components.}
\item{center, scale}{the centring and scaling of the input data.}
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

plotPoints(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "black"),
  pch = c(20,17,8,21), pt.bg = "orange", legend = TRUE, legend.pos = "bottomright")
}
