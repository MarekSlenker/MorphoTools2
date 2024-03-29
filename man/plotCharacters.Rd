\name{plotCharacters}
\alias{plotCharacters}

\title{Draws Character's Contribution as Arrows}

\description{
The character's contribution to ordination axes are visualised as arrows.
}

\usage{
plotCharacters(result, axes = c(1, 2), xlab = NULL, ylab = NULL,
    main = NULL, xlim = NULL, ylim = NULL, col = "red", length = 0.1,
    angle = 15, labels = TRUE, cex = 0.7, ...)
}

\arguments{
\item{result}{an object of class \code{\link{pcadata}} or \code{\link{cdadata}}.}
\item{axes}{x, y axes of plot.}
\item{xlab, ylab}{a title of the respective axes.}
\item{xlim, ylim}{numeric vectors of length 2, giving the x and y coordinates ranges.}
\item{main}{a main title for the plot.}
\item{col}{the colour for arrows.}
\item{length}{length of the edges of the arrow head (in inches).}
\item{angle}{angle from the shaft of the arrow to the edge of the arrow head.}
\item{labels}{logical, if \code{TRUE}, labels are displayed. Only restricted number of parameters are supported. For more precise labels plotting, use \code{\link{plotAddLabels.characters}} directly.}
\item{cex}{character expansion factor for labels.}
\item{...}{further arguments to be passed to \code{\link{arrows}} or other graphical parameters in \code{\link{par}}.}
}

\value{
None. Used for its side effect of producing a plot.
}


\details{
The distribution of samples in ordination space is driven by morphological characters. Each character has its own contribution to ordination axes. These contributions are visualised as arrows. The direction and length of the arrows characterize the impact of the morphological characters on the separation of objects along a given axis. This information is stored in eigenvectors or total canonical structure coefficients for principal component analysis of canonical discriminant analysis, respectively.

The \code{plotCharacters} method is not applicable to results of the principal coordinates analysis (\code{\link{pcoa.calc}}) and non-metric multidimensional scaling (\code{\link{nmds.calc}}) analyses, as the influence of original characters on new axes can not be directly derived, and variation explained by individual axes is unknown.
}


\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = removePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plotCharacters(pcaRes)
}


