\name{cormat}
\alias{cormat}
\alias{cormatSignifTest}
\title{Correlations of characters}

\description{
cormat calculates the matrix of the correlation coefficients of the characters.
}


\usage{
cormat(object, method = "pearson")

cormatSignifTest(object, method = "pearson", alternative = "two.sided")
}
\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{method}{a character string indicating which correlation coefficient is to be used for the test.
One of "pearson" (default), or "spearman" can be applied.}

\item{alternative}{indicates the alternative hypothesis and must be one of "two.sided" (default),
"greater" (positive association) or "less" (negative association).}
}

\value{
object of class 'data.frame'
}

\details{
This functions return table with pairwise correlation coefficients for each pair of
morphological characters. The results are formatted as data frames to allow export with the \code{\link{exportRes}} function.

Significance tests are usually unnecessary for morphometric analysis. Anyway,
if tests are needed, they can be computed using the \code{\link{cormatSignifTest}} function.
}
\examples{
data(centaurea)

correlations.p = cormat(centaurea, method="pearson")
correlations.s = cormat(centaurea, method="spearman")

exportRes(correlations.p, file="correlations.pearson.txt")
exportRes(correlations.s, file="correlations.spearman.txt")

correlations.p = cormatSignifTest(centaurea, method="pearson")

}
