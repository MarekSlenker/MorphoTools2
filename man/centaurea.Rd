\docType{data}
\name{centaurea}
\alias{centaurea}
\title{25 Morphological Characters of Three Species of the \emph{Centaurea phrygia} Complex}

\usage{
data(centaurea)
}

\description{
The sample data include part of data sets from previously published studies by Koutecky (2007) and Koutecky et al. (2012): 25 morphological characters (see the cited studies for details) of the vegetative (stems and leaves) and reproductive structures (capitula and achenes) of three diploid species of the \emph{Centaurea phrygia} complex: \emph{C. phrygia} L. s.str. (abbreviated ``ph''), \emph{C. pseudophrygia} C.A.Mey. (``ps'') and \emph{C. stenolepis} A.Kern. (``st''). Moreover, a fourth group includes the putative hybrid of the \emph{C. pseudophrygia} and \emph{C. stenolepis} (``hybr''). The data represent 8, 12, 7 and 6 populations for each group, respectively, and 20 individuals per population, with one exception in which only 12 individuals were available. All morphological characters are either quantitative (sizes, counts, or ratios) or binary (two characters states or presence/absence). In four characters of achenes (AL, AW, ALW, AP), there are missing data because fruits were not available in all individuals. In two populations of \emph{C. stenolepis} (LIP, PREL) fruits were completely missing. In total, the data set includes 652 individuals (453 complete) from 33 populations (31 complete).
}


\format{
an object of class \code{\link{morphodata}} with the following elements:

\tabular{lrlrl}{
  \tab \code{ID} \tab IDs of each row of \code{data} object. \cr
  \tab \code{Population} \tab population membership of each row of \code{data} object. \cr
  \tab \code{Taxon} \tab taxon membership of each row of \code{data} object. \cr
  \tab \code{data} \tab \code{data.frame} of individuals (rows) and values of measured morphological characters (columns). \cr
  }
}

\keyword{datasets}

\references{
\strong{Koutecky P. (2007)}. Morphological and ploidy level variation of \emph{Centaurea phrygia} agg.(Asteraceae) in the Czech Republic, Slovakia and Ukraine. \emph{Folia Geobotanica} 42, 77-102.\cr
\strong{Koutecky P., Stepanek J., Badurova T. (2012)}. Differentiation between diploid and tetraploid \emph{Centaurea phrygia}: mating barriers, morphology and geographic distribution. \emph{Preslia} 84, 1-32.}


