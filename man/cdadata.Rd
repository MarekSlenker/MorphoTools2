\docType{data}
\name{cdadata}
\alias{cdadata}
\title{Class cdadata}

\description{
The cdadata class is designed for storing results of Canonical discriminant analysis (CDA).}

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
	
	\item{eigenValues}{eigenvalues, i.e., proportion of variation of the original dataset expressed by individual axes.}
	\item{eigenvaluesAsPercent}{eigenvalues as percent, percentage of their total sum. }
	\item{cumulativePercentageOfEigenvalues}{cumulative percentage of eigenvalues. }
	\item{groupMeans}{data.frame containing the means for the taxa.}
	\item{rank}{number of non-zero eigenvalues.}
	\item{coeffs.raw}{matrix containing the raw canonical coefficients.}
	\item{coeffs.std}{matrix containing the standardized canonical coefficients.}
	\item{totalCanonicalStructure}{matrix containing the total canonical structure coefficients, i.e., total-sample correlations between the original variables and the canonical variables.}
	\item{canrsq}{squared canonical correlations.}

	}
}

