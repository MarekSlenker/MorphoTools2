\name{plot.3Dpoints}
\alias{plot.3Dpoints}
\alias{plot.3Dpoints.pcadata}
\alias{plot.3Dpoints.cdadata}
\title{The default scatterplot 3D function}

\description{
A generic function for ploting ordination scores stored in 'pcadata' and 'cdadata' objects.
}


\usage{
plot.3Dpoints(result, axes = c(1,2,3), xlab=NULL, ylab=NULL, zlab=NULL,
  pch = 16, col = "black", pt.bg = "white", phi = 10, theta = 2,
  ticktype = "detailed", bty = "u", type = "p", labels = FALSE,
 legend = FALSE, legend.pos = "topright", ncol = 1, ...)
}


\arguments{
\item{result}{object of class '\code{\link{pcadata}}' or '\code{\link{cdadata}}'.}
\item{axes}{x, y, z axes of plot.}
\item{xlab, ylab, zlab}{a title of the respective axes.}
\item{pch}{a vector of plotting characters or symbols: see \code{\link{points}}.}
\item{col}{the colors for points. Multiple colors can be specified so that each Taxon can be given its own color. If there are fewer colors than points they are recycled in the standard fashion.}
\item{pt.bg}{the background colors for points. Multiple colors can be specified, as above.}
\item{theta, phi}{the angles defining the viewing direction. theta gives the azimuthal direction and phi the colatitude. see \code{\link{persp}}.}
\item{ticktype}{character: "'\code{simple}'" draws just an arrow parallel to the axis to indicate direction of increase; "'\code{detailed}'" draws normal ticks as per 2D plots.}
\item{bty}{The type of the box. One of \code{"g", "b2", "bl" "f" "u"} can be specified.}
\item{type}{the type of plot points, \code{"p"} for points, or \code{"h"} for vertical spikes.}
\item{labels}{logical, defines if point's labels are displayed.}
\item{legend}{logical, defines if legend is displayed. Only restricted number of legend parameters are supported. For more precise legend ploting, use \code{\link{plot.addLegend}} directly.}
\item{legend.pos}{a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.}
\item{ncol}{the number of columns in which to set the legend items (default is 1, a vertical legend).}
\item{...}{further arguments to be passed to \code{\link{plot3D}}, \code{\link{persp}}, \code{\link{par}}.}

}



\examples{
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plot.3Dpoints(pcaRes,col = c("red","green","blue","black"), pch=c(20,17,8,21), pt.bg = "orange")
}






