\name{cormat}
\alias{cormat}
\alias{cormatSignifTest}
\title{Correlations of Characters}

\description{
The cormat function calculates the matrix of the correlation coefficients of the characters.
}


\usage{
cormat(object, method = "pearson")

cormatSignifTest(object, method = "pearson", alternative = "two.sided")
}
\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{method}{a character string indicating which correlation coefficient is to be used for the test.
One of \code{"pearson"} (default), or \code{"spearman"} can be applied.}

\item{alternative}{indicates the alternative hypothesis and must be one of \code{"two.sided"} (default),
\code{"greater"} (positive association) or \code{"less"} (negative association).}
}

\value{
an object of class '\code{\link{data.frame}}'
}

\details{
This function returns table with pairwise correlation coefficients for each pair of
morphological characters. The results are formatted as data frames to allow export with the \code{\link{exportRes}} function.

Significance tests are usually unnecessary for morphometric analysis. Anyway,
if tests are needed, they can be computed using the \code{\link{cormatSignifTest}} function.
}
\examples{
data(centaurea)

correlations.p = cormat(centaurea, method="pearson")
correlations.s = cormat(centaurea, method="spearman")

exportRes(correlations.p, file="~/correlations.pearson.txt")
exportRes(correlations.s, file="~/correlations.spearman.txt")

correlations.p = cormatSignifTest(centaurea, method="pearson")

}
