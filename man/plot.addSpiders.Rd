\name{plot.addSpiders}
\alias{plot.addSpiders}

\title{Add spiders to a plot}

\description{
This function connects taxa's points with its centroids, thus forms a "spider" diagram.
}

\usage{

plot.addSpiders(result, axes = c(1,2), col = "black",
                lty = 1, lwd = 1, ...)

}
\arguments{

\item{result}{an result of \code{\link{pca.calc}} or \code{\link{cda.calc}}, has to by ploted at first.}
\item{axes}{x, y axes of plot.}
\item{col}{the colors for labels.}
\item{lty}{The line type. Line types can either be specified as one of following types: 0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash.}
\item{lwd}{the line width.}
\item{...}{further arguments to be passed to \code{\link{lines}} or other graphical parameters in \code{\link{par}}.}
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
            legend=FALSE, xlim=c(-5,7.5), ylim = c(-5,5.5))

plot.addLegend(pcaRes, col = c("red","green","blue","black"), ncol = 2)

plot.addSpiders(pcaRes, col = c("red","green","blue","black"))


plot.points(pcaRes, col = c("red", "green", "blue","black"), legend=TRUE, cex = 0.4)

plot.addSpiders(pcaRes,  col = c(rgb(255,0,0,max=255,alpha=150), # red
                                  rgb(0,255,0,max=255,alpha=150), # green
                                  rgb(0,0,255,max=255,alpha=150), # blue
                                  rgb(0,0,0,max=255,alpha=150))) # black

}
