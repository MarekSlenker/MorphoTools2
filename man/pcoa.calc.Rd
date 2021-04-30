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
\item{distMethod}{the distance measure to be used. This must be one of: \code{"euclidean"}, \code{"manhattan"}, \code{"minkowski"}, \code{"jaccard"}, \code{"simpleMatching"}, or \code{"gower"}. See details.}
}

\details{
The \code{pcoa.calc} function performs principal coordinates analysis using the \code{\link{cmdscale}} function from package \code{\link{stats}}.
Principal coordinates analysis estimates coordinates for a set of objects in a space. Distances among objects is approximationy of the dissimilarities, based on any similarity or distance coefficient.

Various measures of distance between the observations (rows) are applicable: (1) coefficients of distance for quantitative and binary characters: Euclidean (\code{"euclidean"}), Manhattan (\code{"manhattan"}), Minkovski (\code{"minkowski"}); (2) similarity coefficients for binary characters: Jaccard (\code{"jaccard"}) and simple matching (\code{"simpleMatching"}); (3) coefficient for mixed data: Gower (\code{"gower"}).


GOWEEEEEERRRRR   DETAILY
}


\value{
an object of class '\code{\link{pcoadata}}'.
}



\examples{
data(centaurea)

pcoRes = pcoa.calc(centaurea, distMethod = "manhattan")

summary(pcoRes)

plotPoints(pcoRes, axes = c(1,2), col = c("red","green","blue","black"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE, legend.pos="bottomright")

}
