\name{cda.calc}

\alias{cda.calc}



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

\code{\link{plot.addLegend}}(result, x = "topright", y = NULL, pch = 16, col = "black",
    pt.bg = "white", pt.cex = cex, pt.lwd = par("lwd"), x.intersp = 1,
    y.intersp = 1, box.type = "o", box.lty = par("lty"), box.lwd = par("lwd"),
    box.col = par("fg"), box.bg = "white", cex = 1, ncol = 1, horiz = FALSE, ...)

\code{\link{plot.addEllipses}}(result, axes = c(1,2), probability = 0.95, col = "black",
                  type = "l", lty = 1, lwd = 1, ...)

\code{\link{plot.addSpiders}}(result, axes = c(1,2), col = "black",
                  type = "l", lty = 1, lwd = 1, ...)

\code{\link{plot.characters}}(result, axes = c(1,2), xlab , ylab, xlim, ylim, main,
            col = "red", length = 0.1, angle = 15, labels = TRUE, ...)

\code{\link{plot.addLabels.characters}}(result, labels = rownames(result$totalCanonicalStructure),
    include = T, axes = c(1,2), pos = NULL, offset = 0.5, cex = 0.7, col = NULL, ...)


\code{\link{plot.3Dpoints}}(result, axes = c(1,2,3), xlab = NULL, ylab = NULL, zlab = NULL,
            pch = 16, col = "black", pt.bg = "white", phi = 10, theta = 2,
            ticktype = "detailed", bty = "u", type = "p", labels = FALSE,
            legend = FALSE, legend.pos = "topright", ncol = 1, ...)
}


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
