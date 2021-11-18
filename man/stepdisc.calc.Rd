\name{stepdisc.calc}
\alias{stepdisc.calc}

\title{Stepwise Discriminant Analysis}

\description{
This function perform stepwise discriminant analysis.
}

\usage{
stepdisc.calc(object, FToEnter = 0.15, FToStay = 0.15)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{FToEnter}{significance levels for a variable to enter the subset.}
\item{FToStay}{significance levels for a variable to stay in the subset.}
}

\value{
None. Used for its side effect.
}

\details{
The \code{stepdisc.calc} function performs a stepwise discriminant analysis to select the ``best'' subset of the quantitative variables for use in discriminating among the groups (taxa).
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

stepdisc.calc(centaurea)
}








