\name{clust}
\alias{clust}
\title{Hierarchical Clustering}

\description{
Hierarchical cluster analysis of objects.
}

\usage{
clust(object, distMethod = "euclidean", clustMethod = "UPGMA")
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{distMethod}{the distance measure to be used. This must be one of \code{"euclidean"} (default)

............................... See details.}

\item{clustMethod}{the agglomeration method to be used: \code{"average"} (= \code{"UPGMA"}; default), \code{"complete"},
\code{"ward.D"} (= \code{"Ward"}), \code{"ward.D2"}, \code{"single"}, \code{"mcquitty"} (= \code{"WPGMA"}), \code{"median"} (= \code{"WPGMC"}) or \code{"centroid"} (= \code{"UPGMC"}). See \code{\link{hclust}} for details.
}
}

\value{
an object of class '\code{\link{hclust}}'
}

\details{
This function performs agglomerative hierarchical clustering.
Typically, populations are used as OTUs (operational taxonomic units). Characters are standardised to a zero mean and a unit standard deviation.

Note that the other than default methods for clustering and distance measurement are rarely used in morphometric analyses.

Dendrograms are produced using \code{plot} function.
}
\examples{
data(centaurea)

clustering.UPGMA = clust(centaurea)

plot(clustering.UPGMA, cex=0.6, frame.plot=TRUE, hang=-1,
        main="", sub="", xlab="", ylab="distance")

}
