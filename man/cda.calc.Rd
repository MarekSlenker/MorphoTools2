\name{cda.calc}
\alias{cda.calc}

\title{Canonical Discriminant Analysis}

\description{
This function performs canonical discriminant analysis.
}

\usage{
cda.calc(object, passiveSamples = NULL)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{passiveSamples}{taxa or populations, which will be only predicted, see Details.}
}

\value{
an object of class \code{\link{cdadata}} with the following elements:

\item{objects}{}
\tabular{lrl}{
  \tab \code{ID} \tab IDs of each row of \code{scores} object. \cr
  \tab \code{Population} \tab population membership of each row of \code{scores} object. \cr
  \tab \code{Taxon} \tab taxon membership of each row of \code{scores} object. \cr
  \tab \code{scores} \tab ordination scores of cases (objects, OTUs). \cr
  }

\item{eigenValues}{eigenvalues, i.e., proportion of variation of the original dataset expressed by individual axes.}
\item{eigenvaluesAsPercent}{eigenvalues as percent, percentage of their total sum. }
\item{cumulativePercentageOfEigenvalues}{cumulative percentage of eigenvalues. }
\item{groupMeans}{\code{data.frame} containing the means for the taxa.}
\item{rank}{number of non-zero eigenvalues.}
\item{coeffs.raw}{matrix containing the raw canonical coefficients.}
\item{coeffs.std}{matrix containing the standardized canonical coefficients.}
\item{totalCanonicalStructure}{matrix containing the total canonical structure coefficients, i.e., total-sample correlations between the original variables and the canonical variables.}
\item{canrsq}{squared canonical correlations.}
}

\details{
The \code{cda.calc} function performs canonical discriminant analysis using the \code{candisc} method from the \code{candisc} package. Canonical discriminant analysis finds linear combination of the quantitative variables that maximize the difference in the mean discriminant score between groups. This function allows exclude subset of samples (\code{passiveSamples}) from computing the discriminant function, and only passively predict them in multidimensional space. This approach is advantageous for testing the positions of ``atypical'' populations (e.g., putative hybrids) or for assessing positions of selected individuals (e.g., type herbarium specimens).
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

cdaRes = cda.calc(centaurea)

summary(cdaRes)

plotPoints(cdaRes, col = c("red", "green", "blue", "red"),
  pch = c(20, 17, 8, 21), pt.bg = "orange", legend = TRUE)
}
