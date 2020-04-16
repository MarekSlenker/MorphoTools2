\name{cda.calc}

\alias{cda.calc}


\title{Canonical discriminant analysis (CDA)}

\description{
This function performs generalized canonical discriminant analysis.
}

\usage{
cda.calc(object, passiveSamples = NULL)

## S3 methods for class 'cdadata'

summary(object)

plot.points(cdaResult, axes = c(1,2), xlab = NULL, ylab = NULL,
      pch = 16, col = "black", pt.bg = "white", breaks = NULL,
      ylim = NULL, labels = FALSE, legend = FALSE,
      legend.pos = "topright", ncol = 1, ...)

plot.3D _________

plot.points__________

}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
\item{passiveSamples}{taxa or populations, which will be only predicted, see Details.}

\item{cdaResult}{object of class 'cdadata'.}
\item{axes}{x, y axes of plot.}
\item{xlab, ylab}{a title of the respective axes.}
\item{pch}{a vector of plotting characters or symbols: see \code{\link{points}}.}
\item{col}{the colors for points. Multiple colors can be specified so that each Taxon can be given its own color. If there are fewer colors than points they are recycled in the standard fashion.}
\item{pt.bg}{the background colors for points. Multiple colors can be specified, as above.}
\item{breaks}{a vector giving the breakpoints between histogram cells.}
\item{ylim}{the range of y axis.}
\item{labels}{logical, defines if labels are displayed. Only restricted number of parameters are supported. For more precise labels ploting, use \code{\link{XXXXXXXXXXXXX____labels.points}} directly.}
\item{legend}{logical, defines if legend is displayed. Only restricted number of legend parameters are supported. For more precise legend ploting, use \code{\link{XXXXXXXXX______-plot.legend}} directly.}
\item{legend.pos}{the x and y co-ordinates or a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", and "center", to be used to position the legend.}
\item{ncol}{the number of columns in which to set the legend items (default is 1, a vertical legend).}




\item{...}{further arguments to be passed to \code{\link{plot.default}} or other graphical parameters in \code{\link{par}}.}

}

\value{
object of class '\code{\link{cdadata}}'.
}

\details{
Canonical discriminant analysis finds linear combinations of the quantitative variables that
maximize the difference in mean discriminant score between groups. This implementation is based on \code{candisc} procedure and enables to compute canonical scores for objects. In addition, it also allows exclude subset of samples (\code{passiveSamples}) from computing discriminant function, and only passively predict them in multidimensional space. This approach is advantageous for testing the positions of “atypical” populations (e.g.,
putative hybrids) or for assessing the taxonomic positions of selected individuals (e.g., type herbarium
specimens).


}

\examples{
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))

cdaRes = cda.calc(centaurea)

summary(cdaRes)

plot.points(cdaRes, col = c("red","green","blue","red"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=T)


}
