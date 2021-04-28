\name{nmds.calc}
\alias{nmds.calc}

\title{Non-metric multidimensional scaling (NMDS)}

\description{
This function performs Non-metric multidimensional scaling.
}

\usage{
nmds.calc(object, distMethod = "euclidean", k = 3)
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
\item{distMethod}{the distance measure to be used. This must be one of \code{"euclidean"} (default)
............................... See details.}
\item{k}{number of dimensions.}
}


\details{
The \code{nmds.calc} function performs Non-metric multidimensional scaling using the \code{\link{monoMDS}} function from package \code{\link{vegan}}.
The main threat of NMDS is, that this method doesn't preserve distances among objects in the original character space and approximates only the order of the dissimilarities among objects, based on any coefficient of similarity or distance.

Further, multiple runs of the NMDS analysis are needed to ensure that the stable ordination has been reached, as any one run may get "trapped" in local optima which are not representative of true distances.


The \code{stress} value reflects how well the ordination summarizes the observed distances among the samples. A rule of thumb, 0.1-0.2 is considered fairly good, but there is no general rule since the stress is greatly influenced by the number of points
}

\value{
an object of class '\code{\link{nmdsdata}}'.
}



\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

nmdsRes = nmds.calc(centaurea, distMethod = "euclidean", k = 3)

summary(nmdsRes)

plotPoints(nmdsRes, axes = c(1,2), col = c("red","green","blue","black"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE, legend.pos="bottomright")

}

