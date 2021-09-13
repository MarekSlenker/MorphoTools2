\name{boxplotCharacter}
\alias{boxplotCharacter}

\alias{boxplotAll}

\title{Box Plots}
\usage{
boxplotCharacter(object, character, outliers = TRUE, lowerWhisker = 0.05,
          upperWhisker = 0.95, col = "white", border = "black", main = character,
          cex.main = 1.5, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8,
          horizontal = FALSE, varwidth = FALSE, ...)

boxplotAll(object, folderName = "boxplots", outliers = TRUE, lowerWhisker = 0.05,
      upperWhisker = 0.95, col = "white", border = "black", main = character,
      cex.main = 1.5, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8,
      horizontal = FALSE, varwidth = FALSE, width = 480, height = 480, units = "px", ...)
}
\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}

\item{character}{a morphological character used to plot boxplot.}

\item{folderName}{folder to save produced boxplots.}

\item{outliers}{logical, if \code{TRUE}, the outliers are drawn.}

\item{lowerWhisker}{percentile to which the lower whisker is extended.}

\item{upperWhisker}{percentile to which the upper whisker is extended.}

\item{col}{background colour for the boxes.}

\item{border}{colour of outliers and the lines.}

\item{frame}{logical, if \code{TRUE}, a 'frame' (box around the plot) is drawn.}

\item{main}{main title for the plot.}

\item{cex.main}{magnification to be used for the main title.}

\item{pch}{plotting symbol of the outliers.}

\item{xlab, ylab}{title of the respective axes.}

\item{horizontal}{logical, indicating if the boxplot should be horizontal.}

\item{varwidth}{logical, if \code{TRUE}, the boxes are drawn with widths proportional to the square-roots of the number of observations in the groups.}

\item{width}{the width of the figure.}

\item{height}{the height of the figure.}

\item{units}{the units in which \code{height} and \code{width} are given. Can be \code{"px"} (pixels, the default), \code{"in"} (inches), \code{"cm"} or \code{"mm"}.}

\item{...}{further arguments to be passed to \code{\link{boxplot}} or \code{\link{bxp}}.}

}
\description{
These functions produce a box-and-whisker plot(s) of the given morphological character(s).
}
\details{
These functions modify the classical \code{\link{boxplot}} function to allow whiskers to be extended to the desired percentiles. By default, the whiskers are extended to the 5th and 95th percentiles, because of the trimmed range (without the most extreme 10\% of values) use to be used in taxa descriptions, determination keys, etc. Box defines 25th and 75th percentiles, bold horizontal line shows median (50th percentile). Missing values are ignored.

The \code{boxplotAll} function produces boxplots for each morphological character and saves them to a folder defined by the \code{folderName} argument. If it does not exist, a new folder is created.
}
\examples{
data(centaurea)

boxplotCharacter(centaurea, character = "ST", col = "orange", border = "red")

boxplotCharacter(centaurea, character = "ST", outliers = FALSE,
          lowerWhisker = 0.1, upperWhisker = 0.9)

boxplotCharacter(centaurea, "ST", varwidth = TRUE, notch = TRUE,
          boxwex = 0.4, staplewex = 1.3, horizontal = TRUE)

boxplotCharacter(centaurea, "ST", boxlty = 1, medlwd = 5,
          whisklty = 2, whiskcol = "red", staplecol = "red",
          outcol = "grey30", pch = "-")

boxplotAll(centaurea, folderName = "../boxplots")
}
