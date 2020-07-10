\name{plotCharacters}
\alias{plotCharacters}
\title{Draws character's contribution as arrows}

\description{
The character's contribution to separation of objects are visualised as arrows.
}


\usage{
plotCharacters(result, axes = c(1, 2), xlab = NULL, ylab = NULL,
    main = NULL, xlim = NULL, ylim = NULL, col = "red", length = 0.1,
    angle = 15, labels = TRUE, cex = 0.7, ...)
}


\arguments{
\item{result}{object of class '\code{\link{pcadata}}' or '\code{\link{cdadata}}'.}


\item{axes}{x, y axes of plot.}
\item{xlab, ylab}{a title of the respective axes.}
\item{xlim, ylim}{numeric vectors of length 2, giving the x and y coordinates ranges.}
\item{main}{an overall title for the plot.}
\item{col}{the color for arrows.}
\item{length}{length of the edges of the arrow head (in inches).}
\item{angle}{angle from the shaft of the arrow to the edge of the arrow head.}
\item{labels}{logical, defines if labels are displayed. Only restricted number of parameters are supported. For more precise labels ploting, use \code{\link{plotAddLabels.characters}} directly.}
\item{cex}{character expansion factor for labels.}
\item{...}{further arguments to be passed to \code{\link{arrows}} or other graphical parameters in \code{\link{par}}.}

}

\details{
The separation of samples in ordination space is driven by morphological characters. Each character has its own contribution to separation of objects. These contributions are visualised as arrows. The direction and length of the arrows characterize the impact of the morphological characters to the separation of objects along a given axes. This information is stored in eigenvectors or total canonical structure coefficients for Principal component analysis of Canonical discriminant analysis, respectively.
}


\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plotCharacters(pcaRes)



}


