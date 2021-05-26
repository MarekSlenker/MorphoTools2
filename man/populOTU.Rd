\name{populOTU}
\alias{populOTU}
\title{Population Means}
\usage{
populOTU(object)
}
\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
}
\value{
an object of class \code{\link{morphodata}}
}
\description{
This function calculates the average value for each character in each population, with the pairwise deletion of missing data.
}
\details{
This function returns morphodata object, where each population is used as the operational taxonomic unit (OTUs),
 thus is represented by single "individual" (row) with average values for each character.
Note that when using populations as OTUs, they are handled with the same weight in all analyses
(disregarding population size, within-population variation, etc.)
}
\examples{
data(centaurea)

pops = populOTU(centaurea)

}
