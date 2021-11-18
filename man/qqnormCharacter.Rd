\name{qqnormCharacter}
\alias{qqnormCharacter}
\alias{qqnormAll}
\title{Quantile-Quantile Plots}

\description{
Q-Q plots are produced for the level of taxa/groups, to displays a deviation of morphological characters of each taxon from the normal distribution (line).
}

\usage{
qqnormCharacter(object, character, taxon = levels(object$Taxon), main = NULL, ...)

qqnormAll(object, folderName = "qqnormPlots", taxon = levels(object$Taxon),
          main = NULL, width = 480, height = 480, units = "px", ...)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{character}{a morphological character used to plot Q-Q plot.}
\item{folderName}{folder to save produced Q-Q plots.}
\item{taxon}{taxa which should be plotted, default is to plot all of the taxa.}
\item{main}{main title for the plot.}
\item{width}{the width of the figure.}
\item{height}{the height of the figure.}
\item{units}{the units in which \code{height} and \code{width} are given. Can be \code{"px"} (pixels, the default), \code{"in"} (inches), \code{"cm"} or \code{"mm"}.}
\item{...}{further arguments to be passed to \code{\link{qqnorm}} or graphical parameters \code{\link{par}}.}
}

\value{
None. Used for its side effect of producing a plot(s).
}



\examples{
data(centaurea)

qqnormCharacter(centaurea, character = "SF")

\dontrun{qqnormAll(centaurea, folderName = "../qqnormPlots")}
}
