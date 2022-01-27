\name{plotAddEllipses}
\alias{plotAddEllipses}

\title{Add Prediction Ellipses to a Plot}

\description{
This function draws prediction ellipses around taxa.
}

\usage{
plotAddEllipses(result, axes = c(1,2), probability = 0.95, col = "black",
                  type = "l", lty = 1, lwd = 1, ...)
}

\arguments{
\item{result}{result of \code{\link{pca.calc}}, \code{\link{pcoa.calc}}, \code{\link{nmds.calc}}, or \code{\link{cda.calc}}, has to by plotted at first.}
\item{axes}{x, y axes of plot.}
\item{probability}{probability, that a new independent observation from the same population will fall in that ellipse.}
\item{col}{the colours for labels.}
\item{type}{character indicating the type of plotting, for details, see \code{\link{plot}}: \code{"p"} for points, \code{"l"} for lines, \code{"b"} for both points and lines, \code{"c"} for empty points joined by lines, \code{"o"} for overplotted points and lines, \code{"s"} and \code{"S"} for stair steps and \code{"h"} for histogram-like vertical lines.}
\item{lty}{the line type. Line types can either be specified as one of following types: \code{0}=blank, \code{1}=solid (default), \code{2}=dashed, \code{3}=dotted, \code{4}=dotdash, \code{5}=longdash, \code{6}=twodash.}
\item{lwd}{the line width.}
\item{...}{further arguments to be passed to \code{\link{lines}} or other graphical parameters in \code{\link{par}}.}
}

\value{
None. Used for its side effect of adding elements to a plot.
}

\details{
Prediction ellipses with given \code{probability} define the regions where will fall any new independent observation from the respective taxa. The prediction ellipses are quantified using covariance matrices of taxa scores and chi-squared distribution with two degrees of freedom (Friendly et al. 2013).
}

\references{
\strong{Friendly M., Monette G., Fox J. (2013)}. Elliptical insights: understanding statistical methods through elliptical geometry. \emph{Statistical Science} 28, 1-39.
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = removePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plotPoints(pcaRes, col = c(rgb(255, 0, 0, max = 255, alpha = 150), # red
                           rgb(0, 255, 0, max = 255, alpha = 150), # green
                           rgb(0, 0, 255, max = 255, alpha = 150), # blue
                           rgb(0, 0, 0, max = 255, alpha = 150)), # black
            legend = FALSE, xlim = c(-5, 7.5), ylim = c(-5, 5.5))

plotAddLegend(pcaRes, col = c("red", "green", "blue", "black"), ncol = 2)

plotAddEllipses(pcaRes, col = c("red", "green", "blue", "black"), lwd = 3)
}
