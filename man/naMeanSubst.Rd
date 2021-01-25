\name{naMeanSubst}
\alias{naMeanSubst}
\title{Replace missing data by population average}
\usage{
naMeanSubst(object)
}
\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
}
\value{
an object of class '\code{\link{morphodata}}'
}
\description{
This function substitutes missing data using the average value of the respective character in the respective population.
}
\details{
Generally, most of the multivariate analyses require a full data matrix.
The preferred approach is to reduce the data set to complete observations only (i.e. perform the casewise deletion of
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
