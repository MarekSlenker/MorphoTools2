\name{shapiroWilkTest}
\alias{shapiroWilkTest}
\title{Shapiro-Wilk Normality Test}

\description{
Calculates the Shapiro-Wilk normality test of characters for taxa .
}

\usage{
shapiroWilkTest(object, p.value = 0.05)
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{p.value}{a number or '\code{NA}' are acceptable. In the case of number, the output will be formatted as "normally distributed" or "NOT normally distributed". In the case of '\code{NA}', exact p-values will be returned.}
}

\details{
The normality of within-group distribution is the assumption of Pearson correlation coefficients. Similarly, discriminant analyses require a normal distribution of characters; nevertheless, they have been shown to be considerably robust to deviations in this respect (Thorpe, 1976; Klecka, 1980).
}

\examples{
data(centaurea)

sW = shapiroWilkTest(centaurea)

exportRes(sW)

sW = shapiroWilkTest(centaurea, p.value = NA)

exportRes(sW)
}


\references{
\strong{Klecka WR. 1980}. \emph{Discriminant analysis (No. 19)}. Sage University Paper Series on Quantitative Applications in the Social Sciences 07-019.\cr
\strong{Thorpe RS. 1976}. Biometric analysis of geographic variation and racial affinities. \emph{Biological Reviews of the Cambridge Philosophical Society}, \strong{51}: 407–425.}





