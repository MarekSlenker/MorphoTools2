\name{plotBiplot}
\alias{plotBiplot}

\title{The Default Biplot Function}

\description{
A generic function for plotting ordination scores and the character's contribution to ordination axes in a single plot.
}

\usage{
plotPoints(result, axes = c(1,2), xlab = NULL, ylab = NULL,
      pch = 16, col = "black", pt.bg = "white", breaks = 1,
      xlim = NULL, ylim = NULL, labels = FALSE, arrowLabels = TRUE,
      colArrowLabels = "black", angle = 15, length = 0.1, arrowCol = "red",
      legend = FALSE, legend.pos = "topright", ncol = 1, ...)
}

\arguments{
\item{result}{an object of class \code{\link{pcadata}}, or \code{\link{cdadata}}.}
\item{axes}{x, y axes of plot.}
\item{xlab, ylab}{a title of the respective axes.}
\item{pch}{a vector of plotting characters or symbols: see \code{\link{points}}.}
\item{col}{the colours for points. Multiple colours can be specified so that each taxon can be given its own colour. If there are fewer colours than taxa, they are recycled in the standard fashion.}
\item{pt.bg}{the background colours for points. Multiple colours can be specified, as above.}
\item{breaks}{a numeric, giving the width of one histogram bar.}
\item{xlim, ylim}{the range of x and y axes.}
\item{labels}{logical, if \code{TRUE}, object's labels are displayed.}
\item{arrowLabels}{logical, if \code{TRUE}, character's labels are displayed.}
\item{colArrowLabels}{the colours for character's labels.}
\item{angle}{angle from the shaft of the arrow to the edge of the arrow head.}
\item{length}{length of the edges of the arrow head (in inches).}
\item{arrowCol}{the colour for arrows.}
\item{legend}{logical, if \code{TRUE}, legend is displayed. Only restricted number of legend parameters are supported. For more precise legend plotting, use \code{\link{plotAddLegend}} directly.}
\item{legend.pos}{a single keyword from the list \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"}, and \code{"center"}, to be used to position the legend.}
\item{ncol}{the number of columns in which to set the legend items.}
\item{...}{further arguments to be passed to \code{\link{plot.default}} or other graphical parameters in \code{\link{par}}.}
}

\value{
None. Used for its side effect of producing a plot.
}

\details{
This generic method holds separate implementations of plotting biplots for \code{\link{pcadata}}, and \code{\link{cdadata}} objects.
If only one axis exists, sample scores are displayed as a histogram.
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = removePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plotBiplot(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "red"),
  pch = c(20, 17, 8, 21), pt.bg = "orange", legend = TRUE, legend.pos = "bottomright")

plotBiplot(pcaRes, main = "My PCA plot", cex = 0.8)

cdaRes = cda.calc(centaurea)

plotBiplot(cdaRes, col = c("red", "green", "blue", "red"),
  pch = c(20, 17, 8, 21), pt.bg = "orange", legend = TRUE)
}






