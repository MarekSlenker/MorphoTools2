\docType{data}
\name{classifdata}
\alias{classifdata}
\title{Class classifdata}

\description{
The \code{classifdata} class is designed for storing results of classificatory discriminant analysis.}

\format{Class \code{classifdata}.
\describe{
	\item{ID}{IDs of each row.}
	\item{Population}{population membership of each row.}
	\item{Taxon}{taxon membership of each row.}
	\item{classif}{classification from discriminant analysis.}
	\item{prob}{posterior probabilities of classification into each taxon (if calculated by \code{\link{classif.lda}} or \code{\link{classif.qda}}),
	or proportion of the votes for the winning class (calculated by \code{\link{classif.knn}})}
	\item{correct}{logical, correctness of classification.}

	}
}
