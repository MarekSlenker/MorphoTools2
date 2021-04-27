\name{cda.calc}

\alias{cda.calc}

\title{Canonical Discriminant Analysis}

\description{
This function performs Canonical discriminant analysis.
}

\usage{
cda.calc(object, passiveSamples = NULL)
}


\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
\item{passiveSamples}{taxa or populations, which will be only predicted, see Details.}
}


\value{
an object of class '\code{\link{cdadata}}'.
}

\details{
The \code{cda.calc} function performs Canonical discriminant analysis using the \code{candisc} method from the \code{candisc} package. Canonical discriminant analysis finds linear combination of the quantitative variables that maximize the difference in the mean discriminant score between groups. This function allows exclude subset of samples (\code{passiveSamples}) from computing the discriminant function, and only passively predict them in multidimensional space. This approach is advantageous for testing the positions of “atypical” populations (e.g., putative hybrids) or for assessing positions of selected individuals (e.g., type herbarium specimens).
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

cdaRes = cda.calc(centaurea)

summary(cdaRes)

plotPoints(cdaRes, col = c("red","green","blue","red"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE)

}
