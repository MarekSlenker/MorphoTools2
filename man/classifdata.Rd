\docType{data}
\name{classifdata}
\alias{classifdata}
\title{Class classifdata}

\description{
The classifdata class is designed for storing results of Classificatory discriminant analysis.}

\format{Class \code{classifdata}.
\describe{
	\item{ID}{IDs of each row.}
	\item{Population}{population membership of each row.}
	\item{Taxon}{taxon membership of each row.}

	\item{classif}{classification from discriminant analysis.}

	\item{prob}{probabilities of classification into each group (taxon).}

	\item{correct}{logical, correctness of classification.}

	}
}
