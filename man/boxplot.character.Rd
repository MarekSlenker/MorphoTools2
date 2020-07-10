\name{boxplotCharacter}
\alias{boxplotCharacter}

\alias{boxplotAll}

\title{Box Plots}
\usage{
boxplotCharacter(object, character, outliers = TRUE, lowerWhisker = 0.05,
          upperWhisker = 0.95, col = "white", border = "black", main = character,
          cex.main = 2, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8,
          horizontal = FALSE, varwidth = FALSE, ...)

boxplotAll(object, folderName = "boxplots", outliers = TRUE, lowerWhisker = 0.05,
          upperWhisker = 0.95, col = "white", border = "black", main = character,
          cex.main = 2, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8,
          horizontal = FALSE, varwidth = FALSE, ...)
}
\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{character}{a morphological character used for plotting boxplot.}

\item{folderName}{the name of folder, where to save produces boxplot}

\item{outliers}{logical, if \code{TRUE}, the outliers are drawn.}

\item{lowerWhisker}{a percentile to which the lower whisker is extended.}

\item{upperWhisker}{a percentile to which the upper whisker is extended.}

\item{col}{fill colour, background colour for the boxes.}

\item{border}{a colour of outliers and the boxplots.}

\item{frame}{logical, if \code{TRUE}, a 'frame' (box around the plot) should be drawn.}

\item{main}{a main title for the plot.}

\item{cex.main}{a magnification to be used for the main title.}

\item{pch}{a plotting symbol of the outliers.}

\item{xlab, ylab}{a title of the respective axes.}

\item{horizontal}{logical, indicating if the boxplot should be horizontal.}

\item{varwidth}{logical, if \code{TRUE}, the boxes are drawn with widths proportional to the square-roots of the number of observations in the groups.}

\item{...}{further arguments to be passed to \code{\link{boxplot}} or \code{\link{bxp}} {graphics}.}

}
\description{
These functions produce a box-and-whisker plot(s) of the given morphological characters.
}
\details{
These functions are wrappers above classical R \code{\link{boxplot}} {graphics} function, allowing the user to extend whiskers to desired percentiles. By default, the whiskers are extended to the 5\% and 95\% percentiles, because of the trimmed range (without the most extreme 10% of values) use to be used in taxa descriptions, determination keys, etc. Rectangle defines 25th and 75th percentiles, bold horizontal line show median. The missing values are ignored.

The \code{boxplotAll} function produce boxplot for each morphological character and save them to a folder named in the folderName parameter. If it does not exist, a new folder is created.
}
\examples{
data(centaurea)

boxplotCharacter(centaurea, character= "ST", col = "orange", border = "red")

boxplotCharacter(centaurea, character= "ST", outliers = FALSE,
          lowerWhisker = 0.1, upperWhisker = 0.9)

boxplotCharacter(centaurea, "ST", varwidth = TRUE, notch = TRUE,
          boxwex = 0.4, staplewex = 1.3, horizontal = TRUE)

boxplotCharacter(centaurea, "ST", boxlty = 1, medlwd = 5,
          whisklty = 2, whiskcol = "red", staplecol = "red",
          outcol = "grey30", pch = "-")

boxplotAll(centaurea, folderName = "boxplots")
}
