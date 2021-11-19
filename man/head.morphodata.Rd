\name{head.morphodata}
\alias{head.classifdata}
\alias{tail.classifdata}
\alias{head.morphodata}
\alias{tail.morphodata}

\title{Return the First or Last Parts of an Object}

\description{
Returns the first or last parts of a object.
}

\usage{
\method{head}{classifdata}(x, n = 6, ...)

\method{tail}{classifdata}(x, n = 6, ...)

\method{head}{morphodata}(x, n = 6, ...)

\method{tail}{morphodata}(x, n = 6, ...)
}

\arguments{
\item{x}{an object of class \code{\link{morphodata}} or \code{\link{classifdata}}.}
\item{n}{number of rows to print.}
\item{...}{arguments to be passed to or from other methods.}
}

\value{
A \code{data.frame}, containing the first or last \code{n} individuals of the passed object.
}

\details{
Object passed as parameter is formated to \code{data.frame}. A \code{head()} (\code{tail()}) returns the first (last) \code{n} rows when \code{n} >= 0 or all but the last (first) \code{n} rows when \code{n} < 0.
}



\examples{
data(centaurea)

head(centaurea)
tail(centaurea)
}
