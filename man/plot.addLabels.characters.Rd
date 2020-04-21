\name{plot.addLabels.characters}
\alias{plot.addLabels.characters}

\title{Add labels to a plot}
\usage{
## S3 method for class 'pcadata'
plot.addLabels.characters(result, labels = rownames(result$eigenVectors),
    include = T, axes = c(1,2), pos = NULL, offset = 0.5, cex = 0.7, col = NULL, ...))

## S3 method for class 'cdadata'
plot.addLabels.characters(result, labels = rownames(result$totalCanonicalStructure),
    include = T, axes = c(1,2), pos = NULL, offset = 0.5, cex = 0.7, col = NULL, ...))

}
\arguments{

\item{result}{object of class '\code{\link{pcadata}}' or '\code{\link{cdadata}}'.}
\item{labels}{a vector of label names, which should be included / excluded from ploting, see \code{include}.}
\item{include}{logical, specifi if lalels in \code{labels} argument should be ploted or excluded from plotiing.}
\item{axes}{x, y axes of plot.}


\item{pos}{a position specifier for the text. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the point.}
\item{offset}{when pos is specified, this value controls the distance (‘offset’) of the text label from the point in fractions of a character width.}
\item{cex}{numeric character expansion factor.}
\item{col}{the colors for labels.}
\item{...}{further arguments to be passed to \code{\link{text}} or other graphical parameters in \code{\link{par}}.}


}
\description{
This is a generic function for drawing labels to the character arrows of \code{pcadata} and \code{cdadata} objects.
}

\examples{
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))


pcaRes = pca.calc(centaurea)

plot.characters(pcaRes, labels = F)
plot.addLabels.characters(pcaRes, labels = c("MW", "IW", "SFT", "SF", "LW"), pos = 2, cex = 1)
plot.addLabels.characters(pcaRes, labels = c("LLW", "ILW", "LBA"), pos = 4, cex = 1)
plot.addLabels.characters(pcaRes, labels = c("ML", "IV", "MLW"), pos = 1, cex = 1)

}
