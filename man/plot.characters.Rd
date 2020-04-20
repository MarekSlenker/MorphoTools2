\name{plot.characters}
\alias{plot.characters}
\title{Draws characters contribution as arrows}

\description{
The correlation of the characters and ordination axes are visualised as arrows.
}


\usage{
plot.characters(result, axes = c(1,2), xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
                            col = "red", length = 0.1, angle = 15, labels = TRUE, ...)
}


\arguments{
\item{result}{object of class '\code{\link{pcadata}}' or '\code{\link{cdadata}}'.}


\item{axes}{x, y axes of plot.}
\item{xlab, ylab}{a title of the respective axes.}
\item{xlim, ylim}{numeric vectors of length 2, giving the x and y coordinates ranges.}
\item{col}{the color for arrows.}
\item{length}{length of the edges of the arrow head (in inches).}
\item{angle}{angle from the shaft of the arrow to the edge of the arrow head.}




\item{labels}{logical, defines if labels are displayed. Only restricted number of parameters are supported. For more precise labels ploting, use \code{\link{labels.charactersXXXXXXXXXXXX}} directly.}





\item{...}{further arguments to be passed to \code{\link{arrows}} or other graphical parameters in \code{\link{par}}.}

}

\details{
The correlation of the characters and ordination axes are visualised as arrows.
}


\examples{
The correlation of the characters and ordination axes are visualised as arrows.
}


