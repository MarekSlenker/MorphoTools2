\name{deleteTaxon}
\alias{deleteTaxon}
\alias{deletePopulation}
\alias{deleteSample}
\alias{deleteCharacter}

\title{Remove items (Taxa, Populations, morphological characters) from morphodata object}
\usage{
deleteTaxon(object, taxonName)

deletePopulation(object, populationName)

deleteSample(object, sampleName = NULL, missingPercentage = NA)

deleteCharacter(object, characterName)
}
\arguments{
\item{object}{object of class '\code{\link{morphodata}}'.}

\item{taxonName}{vector of taxa to be removed.}

\item{populationName}{vector of populations to be removed.}

\item{sampleName}{vector of samples to be removed.}

\item{missingPercentage}{a numeric, samples holding more missing data than specified by \code{missingPercentage} will be removed.}

\item{characterName}{vector of characters to be removed.}
}


\value{
an object of class '\code{\link{morphodata}}'
}

\description{
These functions remove particular taxa, populations, samples or morphological characters from morphodata object. The samples can be deleted by names using \code{sampleName} argument, or each sample above the desired threshold \code{missingPercentage} will be deleted. Only one parameter can be specified in one run.


}
\examples{
data(centaurea)

centaurea.3tax = deleteTaxon(centaurea, "hybr")
centaurea.PsSt = deleteTaxon(centaurea, c("ph", "hybr"))

centaurea.short = deletePopulation(centaurea, c("LIP", "PREL"))

centaurea.NA_0.1 = deleteSample(centaurea, missingPercentage = 0.1)

centaurea.short = deleteCharacter(centaurea, "LL")
}
