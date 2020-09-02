\name{deleteTaxon}
\alias{deleteTaxon}
\alias{deletePopulation}
\alias{deleteCharacter}
\title{Remove items (Taxa, Populations, morphological characters) from an morphodata object}
\usage{
deleteTaxon(object, taxonName)

deletePopulation(object, populationName)

deleteCharacter(object, characterName)
}
\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{taxonName}{vector of taxa to be removed.}

\item{populationName}{vector of populations to be removed.}

\item{characterName}{vector of characters to be removed.}
}
\value{
object of class '\code{\link{morphodata}}'
}
\description{
This functions remove particular taxa, populations or morphological characters from morphodata object.
}
\examples{
data(centaurea)

centaurea.3tax = deleteTaxon(centaurea, "hybr")
centaurea.PsSt = deleteTaxon(centaurea, c("ph", "hybr"))

centaurea.short = deletePopulation(centaurea, c("LIP", "PREL"))

centaurea.short = deleteCharacter(centaurea, "LL")

}
