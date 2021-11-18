\name{pcoa.calc}
\alias{pcoa.calc}

\title{Principal Coordinates Analysis (PCoA)}

\description{
This function performs principal coordinates analysis.
}


\usage{
pcoa.calc(object, distMethod = "Euclidean", binaryChs = NULL,
               nominalChs = NULL, ordinalChs = NULL)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{distMethod}{the distance measure to be used. This must be one of: \code{"Euclidean"}, \code{"Manhattan"}, \code{"Minkowski"}, \code{"Jaccard"}, \code{"simpleMatching"}, or \code{"Gower"}. See details.}
\item{binaryChs, nominalChs, ordinalChs}{names of categorical ordinal, categorical nominal (multistate), and binary characters. Needed for Gower's dissimilarity coefficient only, see details.}
}

\details{
The \code{pcoa.calc} function performs principal coordinates analysis using the \code{\link{cmdscale}} function from package \code{\link{stats}}.
Principal coordinates analysis estimates coordinates for a set of objects in a space. Distances among objects is approximationy of the dissimilarities, based on any similarity or distance coefficient.

Various measures of distance between the observations (rows) are applicable: (1) coefficients of distance for quantitative and binary characters: \code{"Euclidean"}, \code{"Manhattan"}, \code{"Minkowski"}; (2) similarity coefficients for binary characters: \code{"Jaccard"} and simple matching (\code{"simpleMatching"}); (3) coefficient for mixed data: (\code{"Gower"}).

The Gower's dissimilarity coefficient can handle different types of variables. Characters have to be divided into four categories: (1) quantitative characters, (2) categorical ordinal characters, (3) categorical nominal (multistate) characters, and (4) binary characters. All characters are considered to be quantitative characters unless otherwise specified. Other types of characters have to be explicitly specified. To mark characters as ordinal, nominal, or binary, enumerate them by names using \code{ordinalChs}, \code{nominalChs}, and \code{binaryChs} arguments, respectively.
}

\value{
an object of class \code{\link{pcoadata}} with the following elements:

\item{objects}{}
\tabular{lrl}{
\tab \code{ID} \tab IDs of each row of \code{scores} object. \cr
\tab \code{Population} \tab population membership of each row of \code{scores} object. \cr
\tab \code{Taxon} \tab taxon membership of each row of \code{scores} object. \cr
\tab \code{scores} \tab ordination scores of cases (objects, OTUs). \cr
}

\item{eigenValues}{eigenvalues of principal coordinates.}
\item{eigenvaluesAsPercent}{eigenvalues as percent, percentage of their total sum. }
\item{cumulativePercentageOfEigenvalues}{cumulative percentage of eigenvalues. }
\item{groupMeans}{\code{data.frame} containing the means for the taxa.}
\item{distMethod}{used distance measure.}
\item{rank}{number of possitive eigenvalues.}
}

\examples{
data(centaurea)

pcoRes = pcoa.calc(centaurea, distMethod = "Manhattan")

summary(pcoRes)

plotPoints(pcoRes, axes = c(1,2), col = c("red", "green", "blue", "black"),
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

pcoaGower = pcoa.calc(data, distMethod = "Gower", binaryChs = c("stemBranching"),
                      nominalChs = c("petalColour", "leaves"), ordinalChs = c("taste"))

plotPoints(pcoaGower, axes = c(1,2), col = c("red","green"),
           pch = c(20,17), pt.bg = "orange", legend = TRUE, legend.pos = "bottomright")
}
