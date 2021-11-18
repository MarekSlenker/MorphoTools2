\name{characters}
\alias{characters}

\title{List Morphological Characters}

\description{
Returns list morphological characters of object.
}

\usage{
characters(object)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}, \code{\link{pcadata}} or \code{\link{cdadata}}.}
}

\value{
A character vector containing names of morphological characters of object.
}




\examples{
data(centaurea)

characters(centaurea)
}
