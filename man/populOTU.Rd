\name{populOTU}
\alias{populOTU}
\title{Population Means}

\description{
This function calculates the average value for each character in each population, with the pairwise deletion of missing data.
}

\usage{
populOTU(object)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
}

\value{
an object of class \code{\link{morphodata}} with the following elements:

\item{ID}{IDs of each row of \code{data} object.}
\item{Population}{population membership of each row of \code{data} object.}
\item{Taxon}{taxon membership of each row of \code{data} object.}
\item{data}{\code{data.frame} of individuals (rows) and values of measured morphological characters (columns).}
}

\details{
This function returns \code{morphodata} object, where each population is used as the operational taxonomic unit (OTUs),
 thus is represented by single ``individual'' (row) with average values for each character.
Note that when using populations as OTUs, they are handled with the same weight in all analyses
(disregarding population size, within-population variation, etc.)
}
\examples{
data(centaurea)

pops = populOTU(centaurea)
}
