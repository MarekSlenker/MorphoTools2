\name{naMeanSubst}
\alias{naMeanSubst}

\title{Replace Missing Data by Population Average}

\description{
This function substitutes missing data using the average value of the respective character in the respective population.
}

\usage{
naMeanSubst(object)
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
Generally, most of the multivariate analyses require a full data matrix.
The preferred approach is to reduce the data set to complete observations only (i.e., perform the casewise deletion of
 missing data) or to remove characters for which there are missing values.
 The use of mean substitution, which introduces values that are not present in the original data, is justified only if
  (1) there are relatively few missing values, (2) these missing values are scattered throughout many characters
  (each character includes only a few missing values) and (3) removing all individuals or all characters
  with missing data would unacceptably reduce the data set.
}
\examples{
data(centaurea)

centaurea = naMeanSubst(centaurea)
}
