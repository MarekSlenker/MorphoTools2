\name{shapiroWilkTest}
\alias{shapiroWilkTest}
\title{Shapiro-Wilk Normality Test}

\description{
Calculates the Shapiro-Wilk normality test of characters for taxa.
}

\usage{
shapiroWilkTest(object, p.value = 0.05)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}

\item{p.value}{a number or \code{NA} are acceptable. In the case of number, the output will be formatted as \code{"normally distributed"} or \code{"NOT normally distributed"}. In the case of \code{NA}, exact p-values will be returned.}
}



\examples{
data(centaurea)

sW = shapiroWilkTest(centaurea)

# exportRes(sW, file = "sW_test.txt")

sW = shapiroWilkTest(centaurea, p.value = NA)

# exportRes(sW, file = "sW_test.txt")
}

