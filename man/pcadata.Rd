\docType{data}
\name{pcadata}
\alias{pcadata}
\title{Class pcadata}

\description{
The pcadata class is designed for storing results of Principal component analysis (PCA).}

\format{Class \code{pcadata}.
\describe{
	\item{objects}{
			\describe{
				\item{ID}{IDs of each row of \code{scores} object.}
				\item{Population}{population membership of each row of \code{scores} object.}
				\item{Taxon}{taxon membership of each row of \code{scores} object.}
				\item{scores}{ordination scores of cases (objects, OTUs), centred and scaled.}
			}
	}
	\item{eigenVectors}{matrix of eigenvectors (i.e., a matrix of characters loadings).}
	\item{eigenValues}{eigenvalues of principal components, i.e., proportion of variation of the original dataset expressed by individual axes.}
	\item{eigenvaluesAsPercent}{eigenvalues as percent, percentage of their total sum. }
	\item{cumulativePercentageOfEigenvalues}{cumulative percentage of eigenvalues. }
	\item{groupMeans}{data.frame containing the means for the taxa.}
	\item{rank}{number of principal components.}
	\item{sdev}{the standard deviations of the principal components (i.e., the square roots of the eigenvalues). }
	\item{center, scale}{the centring and scaling of the input data.}

	}
}

