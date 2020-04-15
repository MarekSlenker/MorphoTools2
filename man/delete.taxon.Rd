\name{delete.taxon}
\alias{delete.taxon}
\alias{delete.population}
\alias{delete.charecter}
\title{Remove items (Taxa, Populations, morphological characters) from an morphodata object}
\usage{
delete.taxon(object, taxonName)

delete.population(object, populationName)

delete.charecter(object, charecterName)
}
\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{taxonName}{vector of taxa to be removed.}

\item{populationName}{vector of populations to be removed.}

\item{charecterName}{vector of characters to be removed.}
}
\value{
object of class '\code{\link{morphodata}}'
}
\description{
This functions remove particular taxa, populations or morphological characters from morphodata object.
}
\examples{
data(centaurea)

centaurea.3tax = delete.taxon(centaurea, "hybr")
centaurea.PsSt = delete.taxon(centaurea, c("ph", "hybr"))

centaurea.short = delete.population(centaurea, c("LIP", "PREL"))

centaurea.short = delete.charecter(centaurea, "StemHeight")

}
