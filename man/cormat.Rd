% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlations.R
\name{cormat}
\alias{cormat}
\alias{cormat.signifTest}
\title{Correlations of characters}
\usage{
cormat(object, method)

cormat.signifTest(object, method, alternative)
}
\arguments{
\item{object}{an object of class 'morphodata'.}

\item{method}{a character string indicating which correlation coefficient is to be used for the test.
One of "pearson" (default), "spearman", or "kendall" can be abbreviated.}

\item{alternative}{indicates the alternative hypothesis and must be one of "two.sided" (default),
"greater" (positive association) or "less" (negative association).}
}
\value{
object of class 'data.frame'
}
\description{
cormat calculates the matrix of the correlation coefficients of the characters.
}
\details{
This functions return table with pairwise correlation coefficients for each pair of
morphological characters. The results are formatted as data frames to allow export with the export.res function.

Significance tests are usually unnecessary for morphometric analysis. Anyway,
if tests are needed, they can be computed using the cormat.signifTest() function.
}
\examples{
correlations.p = cormat(myMorphodata, method="pearson")
correlations.s = cormat(myMorphodata, method="spearman")

export.res(correlations.p, file="correlations.pearson.txt")
export.res(correlations.s, file="correlations.spearman.txt")

correlations.p = cormat.signifTest(myMorphodata, method="pearson", alternative = "two.sided")

}