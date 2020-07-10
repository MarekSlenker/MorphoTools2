\name{plotPoints}

\alias{plotPoints}

\title{The default scatterplot function}

\description{
A generic function for ploting ordination scores stored in '\code{\link{pcadata}}' and '\code{\link{cdadata}}' objects.
}

\usage{
plotPoints(result, axes = c(1,2), xlab = NULL, ylab = NULL,
      pch = 16, col = "black", pt.bg = "white", breaks = 1,
      ylim = NULL, labels = FALSE, legend = FALSE,
      legend.pos = "topright", ncol = 1, ...)


}

\arguments{

\item{result}{object of class '\code{\link{pcadata}}' or '\code{\link{cdadata}}'.}
\item{axes}{x, y axes of plot.}
\item{xlab, ylab}{a title of the respective axes.}
\item{pch}{a vector of plotting characters or symbols: see \code{\link{points}}.}
\item{col}{the colors for points. Multiple colors can be specified so that each Taxon can be given its own color. If there are fewer colors than points they are recycled in the standard fashion.}
\item{pt.bg}{the background colors for points. Multiple colors can be specified, as above.}
\item{breaks}{a numeric, giving the lenght of one histogram bar.}
\item{ylim}{the range of y axis.}
\item{labels}{logical, defines if labels are displayed. Only restricted number of parameters are supported. For more precise labels ploting, use \code{\link{plotAddLabels.points}} directly.}
\item{legend}{logical, defines if legend is displayed. Only restricted number of legend parameters are supported. For more precise legend ploting, use \code{\link{plotAddLegend}} directly.}
\item{legend.pos}{a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.}
\item{ncol}{the number of columns in which to set the legend items (default is 1, a vertical legend).}

\item{...}{further arguments to be passed to \code{\link{plot.default}} or other graphical parameters in \code{\link{par}}.}

}


\details{
This generic method holds separate implementations of plotting points
for \code{pcadata} and \code{cdadata} objects.
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plotPoints(pcaRes, axes = c(1,2), col = c("red","green","blue","red"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE, legend.pos="bottomright")

plotPoints(pcaRes, main = "My PCA plot", frame.plot = FALSE, cex = 0.8)

cdaRes = cda.calc(centaurea)

plotPoints(cdaRes, col = c("red","green","blue","red"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE)
}






