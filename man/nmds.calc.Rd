\name{nmds.calc}
\alias{nmds.calc}

\title{Non-metric Multidimensional Scaling (NMDS)}

\description{
This function performs Non-metric multidimensional scaling.
}

\usage{
nmds.calc(object, distMethod = "euclidean", k = 3, binaryChs = NULL,
                    nominalChs = NULL, ordinalChs = NULL)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{distMethod}{the distance measure to be used. This must be one of: \code{"euclidean"}, \code{"manhattan"}, \code{"minkowski"}, \code{"jaccard"}, \code{"simpleMatching"}, or \code{"gower"}. See details.}
\item{k}{number of dimensions.}
\item{binaryChs, nominalChs, ordinalChs}{names of categorical ordinal, categorical nominal (multistate), and binary characters. Needed for Gower's dissimilarity coefficient only, see details.}

}


\details{
The \code{nmds.calc} function performs non-metric multidimensional scaling using the \code{\link{monoMDS}} function from package \code{\link{vegan}}.
The main threat of NMDS is, that this method doesn't preserve distances among objects in the original character space and approximates only the order of the dissimilarities among objects, based on any coefficient of similarity or distance.

Further, multiple runs of the NMDS analysis are needed to ensure that the stable ordination has been reached, as anyone run may get ``trapped'' in local optima which are not representative of true similarities.

The \code{stress} value reflects how well the ordination summarizes the observed relationship among the samples. A rule of thumb, 0.1-0.2 is considered fairly good, but there is no general rule since the stress is greatly influenced by the number of points. Since stress decreases as dimensionality increases, the optimal solution is when the decrease in stress is small after decreasing the number of dimensions.

Various measures of distance between the observations (rows) are applicable: (1) coefficients of distance for quantitative and binary characters: Euclidean (\code{"euclidean"}), Manhattan (\code{"manhattan"}), Minkovski (\code{"minkowski"}); (2) similarity coefficients for binary characters: Jaccard (\code{"jaccard"}) and simple matching (\code{"simpleMatching"}); (3) coefficient for mixed data: Gower (\code{"gower"}).

The Gower's dissimilarity coefficient can handle different types of variables. Characters have to be divided into four categories: (1) quantitative characters, (2) categorical ordinal characters, (3) categorical nominal (multistate) characters, and (4) binary characters. All characters are considered to be quantitative characters unless otherwise specified. Other types of characters have to be explicitly specified. To mark characters as ordinal, nominal, or binary, enumerate them by names using \code{ordinalChs}, \code{nominalChs}, and \code{binaryChs} arguments, respectively.


}

\value{
an object of class \code{\link{nmdsdata}}.
}



\examples{
data(centaurea)

nmdsRes = nmds.calc(centaurea, distMethod = "euclidean", k = 3)

summary(nmdsRes)

plotPoints(nmdsRes, axes = c(1,2), col = c("red", "green", "blue", "black"),
  pch = c(20,17,8,21), pt.bg = "orange", legend = TRUE, legend.pos = "bottomright")



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

nmdsGower = nmds.calc(data, distMethod = "gower", k = 2, binaryChs = c("stemBranching"),
                      nominalChs = c("petalColour", "leaves"), ordinalChs = c("taste"))

plotPoints(nmdsGower, axes = c(1,2), col = c("red","green"),
           pch = c(20,17), pt.bg = "orange", legend = TRUE, legend.pos = "bottomright")




}

