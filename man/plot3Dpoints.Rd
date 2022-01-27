\name{plot3Dpoints}
\alias{plot3Dpoints}

\title{The Default Scatterplot 3D Function}

\description{
A generic function for plotting ordination scores stored in \code{\link{pcadata}}, \code{\link{pcoadata}}, \code{\link{nmdsdata}}, and \code{\link{cdadata}} objects.
}

\usage{
plot3Dpoints(result, axes = c(1,2,3), xlab = NULL, ylab = NULL, zlab = NULL,
  pch = 16, col = "black", pt.bg = "white", phi = 10, theta = 2,
  ticktype = "detailed", bty = "u", type = "p", labels = FALSE,
 legend = FALSE, legend.pos = "topright", ncol = 1, ...)
}

\arguments{
\item{result}{an object of class \code{\link{pcadata}}, \code{\link{pcoadata}}, \code{\link{nmdsdata}}, or \code{\link{cdadata}}.}
\item{axes}{x, y, z axes of plot.}
\item{xlab, ylab, zlab}{a title of the respective axes.}
\item{pch}{a vector of plotting characters or symbols, see \code{\link{points}}.}
\item{col}{the colours for points. Multiple colours can be specified so that each taxon can be given its own colour. If there are fewer colours than taxa, they are recycled in the standard fashion.}
\item{pt.bg}{the background colours for points. Multiple colours can be specified, as above.}
\item{theta, phi}{the angles defining the viewing direction. \code{Theta} gives the azimuthal direction and \code{phi} the colatitude, see \code{\link{persp}}.}
\item{ticktype}{character: \code{"simple"} draws just an arrow parallel to the axis to indicate direction of increase; \code{"detailed"} draws normal ticks as per 2D plots.}
\item{bty}{the type of the box. One of \code{"g", "b2", "bl", "f", "u"} can be specified.}
\item{type}{the type of plot points, \code{"p"} for points, or \code{"h"} for vertical spikes.}
\item{labels}{logical, if \code{TRUE}, point's labels are displayed.}
\item{legend}{logical, if \code{TRUE}, legend is displayed. Only restricted number of legend parameters are supported. For more precise legend plotting, use \code{\link{plotAddLegend}} directly.}
\item{legend.pos}{a single keyword from the list \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"}, and \code{"center"}, to be used to position the legend.}
\item{ncol}{the number of columns in which to set the legend items.}
\item{...}{further arguments to be passed to \code{\link{plot3D}}, \code{\link{persp}}, \code{\link{par}}.}
}

\value{
None. Used for its side effect of producing a plot.
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = removePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plot3Dpoints(pcaRes, col = c("red", "green", "blue", "black"), pch = c(20,17,8,21),
                 pt.bg = "orange")
}






