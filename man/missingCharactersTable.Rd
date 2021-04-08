\name{missingCharactersTable}
\alias{missingCharactersTable}

\title{Summarize Missing Data}

\description{
Summarize percentage and number of missing values on the desired grouping level.
}

\usage{
missingCharactersTable(object, level)
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{level}{level of groping, one of the following: \code{"taxon"}, populations (\code{"pop"}), or individuals (\code{"indiv"})
}
}

\examples{
data(centaurea)

missingCharactersTable(centaurea, level = "pop")

}
