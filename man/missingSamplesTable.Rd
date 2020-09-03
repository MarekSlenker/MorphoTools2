\name{missingSamplesTable}
\alias{missingSamplesTable}

\title{Summarize missing data.}


\description{
Summarize percentage and number of missing characters on the desired grouping level.
}

\usage{
missingSamplesTable(object, level)
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{level}{level of gruping, one of the following: "\code{taxon}", populations ("\code{pop}"), or individuals ("\code{indiv}")
}
}

\examples{
data(centaurea)

missingSamplesTable(centaurea, level = "pop")

}
