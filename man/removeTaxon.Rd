\name{removeTaxon}
\alias{removeTaxon}
\alias{removePopulation}
\alias{removeSample}
\alias{removeCharacter}

\title{Remove Items (Taxa, Populations, Morphological Characters) from Morphodata Object}

\description{
These functions remove particular taxa, populations, samples or morphological characters from \code{morphodata} object. The samples can be deleted by names using \code{sampleName} argument, or each sample above the desired threshold \code{missingPercentage} will be deleted. Only one parameter can be specified in one run.
}

\usage{
removeTaxon(object, taxonName)

removePopulation(object, populationName)

removeSample(object, sampleName = NULL, missingPercentage = NA)

removeCharacter(object, characterName)
}

\arguments{
\item{object}{object of class \code{\link{morphodata}}.}
\item{taxonName}{vector of taxa to be removed.}
\item{populationName}{vector of populations to be removed.}
\item{sampleName}{vector of samples to be removed.}
\item{missingPercentage}{a numeric, samples holding more missing data than specified by \code{missingPercentage} will be removed.}
\item{characterName}{vector of characters to be removed.}
}


\value{
an object of class \code{\link{morphodata}} with the following elements:

\item{ID}{IDs of each row of \code{data} object.}
\item{Population}{population membership of each row of \code{data} object.}
\item{Taxon}{taxon membership of each row of \code{data} object.}
\item{data}{\code{data.frame} of individuals (rows) and values of measured morphological characters (columns).}
}


\examples{
data(centaurea)

centaurea.3tax = removeTaxon(centaurea, "hybr")
centaurea.PsSt = removeTaxon(centaurea, c("ph", "hybr"))

centaurea.short = removePopulation(centaurea, c("LIP", "PREL"))

centaurea.NA_0.1 = removeSample(centaurea, missingPercentage = 0.1)

centaurea.short = removeCharacter(centaurea, "LL")
}
