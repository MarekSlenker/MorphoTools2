\name{cda.calc}

\alias{cda.calc}



\title{Canonical discriminant analysis (CDA)}

\description{
This function performs generalized canonical discriminant analysis.
}

\usage{
cda.calc(object, passiveSamples = NULL)
}


\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
\item{passiveSamples}{taxa or populations, which will be only predicted, see Details.}
}


\value{
object of class '\code{\link{cdadata}}'.
}

\details{
Canonical discriminant analysis finds linear combinations of the quantitative variables that
maximize the difference in mean discriminant score between groups. This implementation is based on \code{candisc} procedure and enables to compute canonical scores for objects. In addition, it also allows exclude subset of samples (\code{passiveSamples}) from computing discriminant function, and only passively predict them in multidimensional space. This approach is advantageous for testing the positions of “atypical” populations (e.g.,
putative hybrids) or for assessing the taxonomic positions of selected individuals (e.g., type herbarium
specimens).


}

\examples{
data(centaurea)
centaurea = na.meanSubst(centaurea)
centaurea = delete.population(centaurea, populationName = c("LIP", "PREL"))

cdaRes = cda.calc(centaurea)

summary(cdaRes)

plot.points(cdaRes, col = c("red","green","blue","red"),
  pch=c(20,17,8,21), pt.bg = "orange", legend=TRUE)

}
