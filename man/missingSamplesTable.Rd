\name{missingSamplesTable}
\alias{missingSamplesTable}

\title{Summarize Missing Data}


\description{
Summarize number of missing values for each character on the desired grouping level.
}

\usage{
missingSamplesTable(object, level)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}

\item{level}{level of grouping, one of the following: \code{"taxon"}, populations (\code{"pop"}), or individuals (\code{"indiv"}.)
}
}

\examples{
data(centaurea)

missingSamplesTable(centaurea, level = "pop")

}
