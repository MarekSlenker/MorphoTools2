\name{plot.addEllipses}
\alias{plot.addEllipses}

\title{Add confidence ellipses to a plot}

\description{
This function draws confidence (prediction) ellipses around taxa.
}

\usage{

plot.addEllipses(result, axes = c(1,2), probability = 0.95, col = "black",
                  type = "l", lty = 1, lwd = 1, ...)

}
\arguments{

\item{result}{an result of \code{\link{pca.calc}} or \code{\link{cda.calc}}, has to by ploted at first.}
\item{axes}{x, y axes of plot.}
\item{probability}{probability, that a new independent observation from the same population will fall in that ellipse.}
\item{col}{the colors for labels.}
\item{type}{character indicating the type of plotting, for details, see \code{\link{plot}}: "p" for points, "l" for lines, "b" for both points and lines, "c" for empty points joined by lines, "o" for overplotted points and lines, "s" and "S" for stair steps and "h" for histogram-like vertical lines. Finally, "n" does not produce any points or lines.}
\item{lty}{The line type. Line types can either be specified as one of following types: 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash.}
\item{lwd}{the line width.}
\item{...}{further arguments to be passed to \code{\link{lines}} or other graphical parameters in \code{\link{par}}.}
}

\details{
Confidence ellipses with given \code{probability} cover areas where will fall any new independent observation from the respective populations. Confidence ellipses are calculated from the estimated means and covariance matrices using chi-squared distribution and given level of probability.

}

\examples{
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plot.points(pcaRes, col = c(rgb(255,0,0,max=255,alpha=150), # red
                            rgb(0,255,0,max=255,alpha=150), # green
                            rgb(0,0,255,max=255,alpha=150), # blue
                            rgb(0,0,0,max=255,alpha=150)), # black
            legend=F, xlim=c(-5,7.5), ylim = c(-5,5.5))

plot.addLegend(pcaRes, col = c("red","green","blue","black"), ncol = 2)

plot.addEllipses(pcaRes, col = c("red","green","blue","black"), lwd = 3)
}
