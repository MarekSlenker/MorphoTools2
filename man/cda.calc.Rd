\name{cda.calc}

\alias{cda.calc}


\title{Canonical discriminant analysis (CDA)}

\description{
This function performs generalized canonical discriminant analysis.
}

\usage{
cda.calc(object)

## S3 methods for class 'morphodata'

plot.3D _________

plot.points__________

}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
}

\value{
object of class '\code{\link{cdadata}}'.
}

\details{

umoznuje passiveSamples - moze to byt TAX alebo POP


}

\examples{
data(centaurea)

cdaResult = cda.calc(centaurea)

}
