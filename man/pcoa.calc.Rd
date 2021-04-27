\name{pcoa.calc}
\alias{pcoa.calc}

\title{Principal coordinates analysis (PCoA)}

\description{
This function performs Principal Coordinates Analysis.
}


\usage{
pcoa.calc(object, distMethod = "euclidean")
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
\item{distMethod}{the distance measure to be used. This must be one of \code{"euclidean"} (default)
............................... See details.}
}

\details{
The \code{pcoa.calc} function performs principal coordinates analysis using the \code{\link{cmdscale}} function from package \code{\link{stats}}.
Principal coordinates analysis estimates coordinates for a set of objects in a space. Distances among objects is approximationy of the dissimilarities, based on any coefficient of similarity or distance.}


\value{
an object of class '\code{\link{pcoadata}}'.
}



\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

pcoRes = pcoa.calc(centaurea, distMethod = "euclidean")

summary(pcoRes)

plotPoints(pcoRes, axes = c(1,2), col = c("red","green","blue","black"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE, legend.pos="bottomright")

}
