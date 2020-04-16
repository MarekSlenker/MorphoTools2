\docType{data}
\name{cdadata}
\alias{cdadata}
\title{Class cdadata}

\description{
The cdadata class is designed for storing results of Canonical discriminant analysis (CDA) and input morphological data.}

\format{Class \code{cdadata}.
\describe{
	\item{objects}{
			\describe{
				\item{ID}{IDs of each row of \code{scores} object.}
				\item{Population}{population membership of each row of \code{scores} object.}
				\item{Taxon}{taxon membership of each row of \code{scores} object.}
				\item{scores}{ordination scores of cases (objects, OTUs), centred and scaled.}
			}
	}
	\item{eigenValues}{eigenvalues.}
	\item{axesVariance}{eigenvalues as percent. }
	\item{cumulativeAxesVariance}{cumulative percentage of eigenvalues. }
	\item{groupMeans}{a data.frame containing the means for the taxa.}
	\item{rank}{a number of non-zero eigenvalues.}
	\item{coeffs.raw}{a matrix containing the raw canonical coefficients.}
	\item{coeffs.std}{a matrix containing the standardized canonical coefficients.}
	\item{totalCanonicalStructure}{a matrix containing the total canonical structure coefficients, i.e., total-sample correlations between the original variables and the canonical variables.}
	\item{canrsq}{a squared canonical correlations.}

	}
}

