\name{cda.calc}

\alias{cda.calc}
\alias{summary.cdadata}


\title{Canonical discriminant analysis (CDA)}

\description{
This function performs generalized canonical discriminant analysis.
}

\usage{
cda.calc(object, passiveSamples = NULL)

## S3 methods for class 'cdadata'

summary(object)

\code{\link{plot.points}}(cdaResult, axes = c(1,2), xlab = NULL, ylab = NULL,
      pch = 16, col = "black", pt.bg = "white", breaks = NULL,
      ylim = NULL, labels = FALSE, legend = FALSE,
      legend.pos = "topright", ncol = 1, ...)

\code{\link{plot.addLabels.points}}(result, labels = result$objects$ID, include = T,
      axes = c(1,2), pos = NULL, offset = 0.5, cex = 1, col = NULL, ...)


plot.3D _________

plot.points__________

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
  pch=c(20,17,8,21), pt.bg = "orange", legend=T)

}
