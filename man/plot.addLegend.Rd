\name{plot.addLegend}
\alias{plot.addLegend}


\title{Add legends to plot}

\description{
This function can be used to add legends to plots.
}

\usage{
plot.addLegend(result, x = "topright", y = NULL, pch = 16, col = "black",
    pt.bg = "white", pt.cex = cex, pt.lwd = par("lwd"), x.intersp = 1,
    y.intersp = 1, box.type = "o", box.lty = par("lty"), box.lwd = par("lwd"),
    box.col = par("fg"), box.bg = "white", cex = 1, ncol = 1, horiz = FALSE, ...)
}

\arguments{
\item{result}{an result of \code{\link{pca.calc}} or \code{\link{cda.calc}}, has to by ploted at first.}
\item{x, y}{the x and y coordinates or a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.}
\item{pch}{the  plotting symbols of points appearing in the legend.}
\item{col}{the color of points appearing in the legend.}
\item{pt.bg}{the background color for the \code{\link{points}}, corresponding to its argument bg.}
\item{pt.cex}{character expansion factor for the points.}
\item{pt.lwd}{the line width for the points.}
\item{x.intersp, y.intersp}{character interspacing factor for horizontal (x) and vertical (y) line distances.}
\item{box.type}{the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n".}
\item{box.lty, box.lwd, box.col, box.bg}{the line type, width color and background color for the legend box (if box.type = "o").}
\item{cex}{character expansion factor for text.}
\item{ncol}{the number of columns in which to set the legend items.}
\item{horiz}{logical; if TRUE, set the legend horizontally rather than vertically (specifying horiz overrides the ncol specification).}
\item{...}{further arguments to be passed to \code{\link{legend}} or other graphical parameters in \code{\link{par}}.}
}

\examples{
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))

pcaRes = pca.calc(centaurea)

plot.points(pcaRes, col = c("red","green","blue","red"),
            pch=c(20,17,8,21), pt.bg = "orange", legend=F)

plot.addLegend(pcaRes, x = "bottomright", col = c("red","green","blue","red"),
               pch=c(20,17,8,21), pt.bg = "orange", ncol = 2)

}








