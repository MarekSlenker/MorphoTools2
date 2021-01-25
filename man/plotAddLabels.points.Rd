\name{plotAddLabels.points}
\alias{plotAddLabels.points}

\title{Add labels to a plot}
\usage{

plotAddLabels.points(result, labels = result$objects$ID, include = T,
      axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...)

}
\arguments{

\item{result}{an result of \code{\link{pca.calc}} or \code{\link{cda.calc}}, has to by plotted at first.}
\item{labels}{a vector of label names, which should be included / excluded from plotting, see \code{include}.}
\item{include}{logical, specify if labels in \code{labels} argument should be plotted or excluded from plotting.}
\item{axes}{x, y axes of plot.}


\item{pos}{a position specifier for the text. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the point.}
\item{offset}{when pos is specified, this value controls the distance (‘offset’) of the text label from the point in fractions of a character width.}
\item{cex}{character expansion factor for text.}
\item{col}{the colors for labels.}
\item{...}{further arguments to be passed to \code{\link{text}} or other graphical parameters in \code{\link{par}}.}


}
\description{
This is a generic function for drawing labels to the data points of \code{pcadata} and \code{cdadata} objects.
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
pops = populOTU(centaurea)


pcaRes = pca.calc(pops)
plotPoints(pcaRes, col = c("red","green","blue","red"),
            pch=c(20,17,8,21), pt.bg = "orange", legend=FALSE)

plotAddLabels.points(pcaRes, labels = c("LES", "BUK", "VOL", "OLE1"), include = TRUE)

plotPoints(pcaRes, col = c("red","green","blue","red"),
            pch=c(20,17,8,21), pt.bg = "orange", legend=FALSE)

plotAddLabels.points(pcaRes, labels = c("LES", "BUK", "VOL", "OLE1"), include = FALSE)

}
