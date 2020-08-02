\name{keepTaxon}
\alias{keepTaxon}
\alias{keepPopulation}
\alias{keepCharecter}

\title{Keep items (Taxa, Populations, morphological characters) in an morphodata object (and remove other)}

\usage{
keepTaxon(object, taxonName)

keepPopulation(object, populationName)

keepCharecter(object, charecterName)
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
This functions keep only selected taxa, populations or morphological characters in morphodata object.
}

\examples{
data(centaurea)

centaurea.hybr = keepTaxon(centaurea, "hybr")
centaurea.PhHybr = keepTaxon(centaurea, c("ph", "hybr"))

centaurea_PREL = keepPopulation(centaurea, "PREL"))

centaurea.stem = keepCharecter(centaurea, c("SN", "SF", "ST"))

}
