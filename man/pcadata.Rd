\docType{data}
\name{pcadata}
\alias{pcadata}
\title{Class pcadata}

\description{
The \code{pcadata} class is designed for storing results of principal component analysis (PCA).}

\format{Class \code{pcadata}.
\describe{
	\item{objects}{
			\describe{
				\item{ID}{IDs of each row of \code{scores} object.}
				\item{Population}{population membership of each row of \code{scores} object.}
				\item{Taxon}{taxon membership of each row of \code{scores} object.}
				\item{scores}{ordination scores of cases (objects, OTUs).}
			}
	}
	\item{eigenVectors}{matrix of eigenvectors (i.e., a matrix of characters loadings).}
	\item{eigenValues}{eigenvalues of principal components, i.e., proportion of variation of the original dataset expressed by individual axes.}
	\item{eigenvaluesAsPercent}{eigenvalues as percent, percentage of their total sum. }
	\item{cumulativePercentageOfEigenvalues}{cumulative percentage of eigenvalues. }
	\item{groupMeans}{\code{data.frame} containing the means for the taxa.}
	\item{rank}{number of principal components.}
	\item{center, scale}{the centring and scaling of the input data.}

	}
}

