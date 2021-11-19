\name{classif.matrix}
\alias{classif.matrix}

\title{Format the Classifdata to Summary Table}

\description{
The \code{classif.matrix} method formats the results stored in \code{classifdata} class to a summary classification table of taxa, populations, or individuals.
}

\usage{
classif.matrix(result, level = "taxon")
}

\arguments{
\item{result}{an object of class \code{\link{classifdata}}.}
\item{level}{level of grouping of classification matrix, \code{"taxon"} (default), populations (\code{"pop"}), or individuals (\code{"indiv"})}
}

\value{
A \code{data.frame}, summary classification table.
}


\examples{
data(centaurea)

centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

# classification by linear discriminant function
classifRes.lda = classif.lda(centaurea, crossval = "indiv")

# exporting results
classif.matrix(classifRes.lda, level = "taxon")
classif.matrix(classifRes.lda, level = "pop")
}
