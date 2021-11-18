\name{keepTaxon}
\alias{keepTaxon}
\alias{keepPopulation}
\alias{keepCharacter}
\alias{keepSample}

\title{Keep Items (Taxa, Populations, Samples, Morphological Characters) in an Morphodata Object (and Remove Others)}

\description{
These functions keep only selected taxa, populations, samples or morphological characters in \code{morphodata} object. The samples can be kept by names using \code{sampleName} argument, or by the threshold. Each sample holding less or equal portion of missing data than the desired threshold (\code{missingPercentage}) will be kept. Only one parameter can be specified in one run.
}

\usage{
keepTaxon(object, taxonName)

keepPopulation(object, populationName)

keepSample(object, sampleName = NULL, missingPercentage = NA)

keepCharacter(object, characterName)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{taxonName}{vector of taxa to be kept.}
\item{populationName}{vector of populations to be kept.}
\item{sampleName}{vector of samples to be kept.}
\item{missingPercentage}{a numeric, samples holding less or equal portion of missing data than specified by \code{missingPercentage} will be kept.}
\item{characterName}{vector of characters to be kept.}
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

centaurea.hybr = keepTaxon(centaurea, "hybr")
centaurea.PhHybr = keepTaxon(centaurea, c("ph", "hybr"))

centaurea.PREL = keepPopulation(centaurea, "PREL")

centaurea.NA_0.1 = keepSample(centaurea, missingPercentage = 0.1)

centaurea.stem = keepCharacter(centaurea, c("SN", "SF", "ST"))
}
