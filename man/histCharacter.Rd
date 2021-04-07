\name{histCharacter}
\alias{histCharacter}
\alias{histAll}
\title{Histograms of characters.}
\usage{
histCharacter(object, character, taxon = levels(object$Taxon), histogram = TRUE,
              col = "lightgray",  densityLine = TRUE, normDistLine = TRUE, ...)

histAll(object, folderName = "histograms", taxon = levels(object$Taxon), histogram = TRUE,
        col = "lightgray", densityLine = TRUE, normDistLine = TRUE,
         width = 480, height = 480, units = "px", ...)
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{character}{a morphological character used to plot histogram.}

\item{folderName}{folder to save produced histograms.}

\item{col}{colour to be used to fill the bars.}


\item{taxon}{taxa which should be plotted, default is to plot all of the taxa.}


\item{histogram}{logical, if \code{TRUE}, the histograms will be drawn.}

\item{densityLine}{logical, if \code{TRUE}, the density line smoothing out the histogram will be drawn.}

\item{normDistLine}{logical, if \code{TRUE}, the normal distribution curve will be drawn.}

\item{width}{the width of the figure.}

\item{height}{the height of the figure.}

\item{units}{the units in which \code{height} and \code{width} are given. Can be \code{px} (pixels, the default), \code{in} (inches), \code{cm} or \code{mm}.}

\item{...}{further arguments to be passed to \code{\link{hist}} or graphical parameters \code{\link{par}}.}

}


\description{
Histograms are produced for the level of taxa/groups, to displays a within-group distribution of each taxon for a particular character, and its deviation from the normal distribution (red line).
}

\examples{
data(centaurea)

histCharacter(centaurea, character = "IW", breaks = seq(0.5, 2.5, 0.1))

histAll(centaurea, folderName = "histograms")
}
