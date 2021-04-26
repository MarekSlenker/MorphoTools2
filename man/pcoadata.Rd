\docType{data}
\name{pcoadata}
\alias{pcoadata}
\title{Class Pcoadata}

\description{
The pcoadata class is designed for storing results of Principal coordinates analysis (PCoA).}

\format{Class \code{pcoadata}.
\describe{
	\item{objects}{
			\describe{
				\item{ID}{IDs of each row of \code{scores} object.}
				\item{Population}{population membership of each row of \code{scores} object.}
				\item{Taxon}{taxon membership of each row of \code{scores} object.}
				\item{scores}{ordination scores of cases (objects, OTUs).}
			}
	}
	\item{eigenValues}{eigenvalues of principal coordinates.}
	\item{eigenvaluesAsPercent}{eigenvalues as percent, percentage of their total sum. }
	\item{cumulativePercentageOfEigenvalues}{cumulative percentage of eigenvalues. }
	\item{groupMeans}{data.frame containing the means for the taxa.}
	\item{distMethod}{used distance measure.}
	\item{rank}{number of possitive eigenvalues.}

	}
}

