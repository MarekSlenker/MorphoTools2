\name{plotAddLegend}
\alias{plotAddLegend}

\title{Add Legend to a Plot}

\description{
This function can be used to add legend to plot.
}

\usage{
plotAddLegend(result, x = "topright", y = NULL, pch = 16, col = "black",
    pt.bg = "white", pt.cex = cex, pt.lwd = 1, x.intersp = 1,
    y.intersp = 1, box.type = "o", box.lty = "solid", box.lwd = 1,
    box.col = "black", box.bg = "white", cex = 1, ncol = 1, horiz = FALSE, ...)
}

\arguments{
\item{result}{result of \code{\link{pca.calc}}, \code{\link{pcoa.calc}}, \code{\link{nmds.calc}}, or \code{\link{cda.calc}}, has to by plotted at first.}
\item{x, y}{the x and y coordinates or a single keyword from the list \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, \code{"right"}, and \code{"center"}, to be used to position the legend.}
\item{pch}{the plotting symbols of points appearing in the legend.}
\item{col}{the colours of points appearing in the legend.}
\item{pt.bg}{the background colour for the \code{\link{points}}, corresponding to its argument \code{bg}.}
\item{pt.cex}{character expansion factor for the points.}
\item{pt.lwd}{the line width for the points.}
\item{x.intersp, y.intersp}{character interspacing factor for horizontal (x) and vertical (y) line distances.}
\item{box.type}{the type of box to be drawn around the legend. The applicable values are \code{"o"} (the default) and \code{"n"}.}
\item{box.lty, box.lwd, box.col, box.bg}{the line type, width colour and background colour for the legend box (if \code{box.type = "o"}).}
\item{cex}{character expansion factor for text.}
\item{ncol}{the number of columns in which to set the legend item.}
\item{horiz}{logical; if \code{TRUE}, set the legend horizontally rather than vertically (specifying horiz overrides the \code{ncol} specification).}
\item{...}{further arguments to be passed to \code{\link{legend}} or other graphical parameters in \code{\link{par}}.}
}

\value{
None. Used for its side effect of adding elements to a plot.
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = removePopulation(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plotPoints(pcaRes, col = c("red", "green", "blue", "red"),
            pch = c(20, 17, 8, 21), pt.bg = "orange", legend = FALSE)

plotAddLegend(pcaRes, x = "bottomright", col = c("red", "green", "blue", "red"),
               pch = c(20, 17, 8, 21), pt.bg = "orange", ncol = 2)
}




