\name{clust}
\alias{clust}
\title{Hierarchical Clustering}

\description{
Hierarchical cluster analysis of objects.
}

\usage{
clust(object, distMethod = "euclidean", clustMethod = "UPGMA", binaryChs = NULL,
              nominalChs = NULL, ordinalChs = NULL)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{distMethod}{the distance measure to be used. This must be one of: \code{"euclidean"} (default), \code{"manhattan"}, \code{"minkowski"}, \code{"jaccard"}, \code{"simpleMatching"}, or \code{"gower"}. See details.}
\item{clustMethod}{the agglomeration method to be used: \code{"average"} (= \code{"UPGMA"}; default), \code{"complete"}, \code{"ward.D"} (= \code{"Ward"}), \code{"ward.D2"}, \code{"single"}, \code{"mcquitty"} (= \code{"WPGMA"}), \code{"median"} (= \code{"WPGMC"}) or \code{"centroid"} (= \code{"UPGMC"}). See \code{\link{hclust}} for details.}

\item{binaryChs, nominalChs, ordinalChs}{names of categorical ordinal, categorical nominal (multistate), and binary characters. Needed for Gower's dissimilarity coefficient only, see details.}




}

\value{
an object of class \code{\link{hclust}}
}

\details{
This function performs agglomerative hierarchical clustering.
Typically, populations are used as OTUs (operational taxonomic units). Characters are standardised to a zero mean and a unit standard deviation.

Various measures of distance between the observations (rows) are applicable: (1) coefficients of distance for quantitative and binary characters: Euclidean (\code{"euclidean"}), Manhattan (\code{"manhattan"}), Minkovski (\code{"minkowski"}); (2) similarity coefficients for binary characters: Jaccard (\code{"jaccard"}) and simple matching (\code{"simpleMatching"}); (3) coefficient for mixed data: Gower (\code{"gower"}).
Note that the other than default methods for clustering and distance measurement are rarely used in morphometric analyses.

The Gower's dissimilarity coefficient can handle different types of variables. Characters have to be divided into four categories: (1) quantitative characters, (2) categorical ordinal characters, (3) categorical nominal (multistate) characters, and (4) binary characters. All characters are considered to be quantitative characters unless otherwise specified. Other types of characters have to be explicitly specified. To mark characters as ordinal, nominal, or binary, enumerate them by names using \code{ordinalChs}, \code{nominalChs}, and \code{binaryChs} arguments, respectively.
}


\examples{
data(centaurea)

clustering.UPGMA = clust(centaurea)

plot(clustering.UPGMA, cex = 0.6, frame.plot = TRUE, hang = -1,
        main = "", sub = "", xlab = "", ylab = "distance")


# using Gower's method
data = list(
    ID = as.factor(c("id1","id2","id3","id4","id5","id6")),
    Population = as.factor(c("Pop1", "Pop1", "Pop2", "Pop2", "Pop3", "Pop3")),
    Taxon = as.factor(c("TaxA", "TaxA", "TaxA", "TaxB", "TaxB", "TaxB")),
    data = data.frame(
     stemBranching = c(1, 1, 1, 0, 0, 0),  # binaryChs
     petalColour = c(1, 1, 2, 3, 3, 3),  # nominalChs; 1=white, 2=red, 3=blue
     leaves = c(1,1,1,2,2,3), # nominalChs; 1=simple, 2=palmately compound, 3=pinnately compound
     taste = c(2, 2, 2, 3, 1, 1),   # ordinal; 1=hot, 2=hotter, 3=hottest
     stemHeight = c(10, 11, 14, 22, 23, 21),         # quantitative
     leafLength = c(8, 7.1, 9.4, 1.2, 2.3, 2.1)  )   # quantitative
)
attr(data, "class") = "morphodata"

clustering.GOWER = clust(data, distMethod = "gower", clustMethod = "UPGMA",
                               binaryChs = c("stemBranching"),
                               nominalChs = c("petalColour", "leaves"),
                               ordinalChs = c("taste"))

plot(clustering.GOWER, cex = 0.6, frame.plot = TRUE, hang = -1,
        main = "", sub = "", xlab = "", ylab = "distance")

}
