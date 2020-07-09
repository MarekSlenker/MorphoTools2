\name{clust}
\alias{clust}
\title{Hierarchical clustering}

\description{
Hierarchical cluster analysis of objects.
}

\usage{
clust(object, distMethod = "euclidean", clustMethod = "average")
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{distMethod}{the distance measure to be used. This must be one of "euclidean" (default), "maximum", "manhattan", "canberra", "binary" or "minkowski". See \code{\link{dist}} for details.}

\item{clustMethod}{the agglomeration method to be used: "average" (= UPGMA; default), "complete",
"ward.D" (= Ward), "ward.D2", "single", "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). See \code{\link{hclust}} for details.}
}


\value{
object of class 'hclust'
}

\details{
This functions performs agglomerative hierarchical clustering.
Typically, populations are used as OTUs. Characters are standardised to a zero mean and a unit standard deviation.

Note that the other then default methods for clustering and distance measurement are rarely used in morphometric analyses.

Dendrograms are produced using plot function.
}
\examples{
data(centaurea)

clustering.UPGMA = clust(centaurea)

plot(clustering.UPGMA, cex=0.6, frame.plot=TRUE, hang=-1,
        main="", sub="", xlab="", ylab="distance")

}
