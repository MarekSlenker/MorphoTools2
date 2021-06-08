\docType{data}
\name{nmdsdata}
\alias{nmdsdata}
\title{Class nmdsdata}

\description{
The \code{nmdsdata} class is designed for storing results of non-metric multidimensional scaling (NMDS).}

\format{Class \code{nmdsdata}.
\describe{
	\item{objects}{
			\describe{
				\item{ID}{IDs of each row of \code{scores} object.}
				\item{Population}{population membership of each row of \code{scores} object.}
				\item{Taxon}{taxon membership of each row of \code{scores} object.}
				\item{scores}{ordination scores of cases (objects, OTUs).}
			}
	}
	\item{stress}{stress value, e.i., goodness of fit.}
	\item{groupMeans}{\code{data.frame} containing the means for the taxa.}
	\item{distMethod}{used distance measure.}
	\item{rank}{number of possitive eigenvalues.}

	}
}

