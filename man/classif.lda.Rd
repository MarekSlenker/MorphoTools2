\name{classif.lda}
\alias{classif.lda}
\alias{knn.select}
\alias{classif.knn}

\title{Classificatory discriminant analysis}


\description{
These functions computes discriminant function for classifying observations. Linear discriminant function (\code{classif.lda}) or nonparametric k-nearest-neighbor classification method (\code{classif.knn}) can be used.
}


\usage{
classif.lda(object, crossval = "indiv")

knn.select(object, crossval = "indiv")
classif.knn(object, k, crossval = "indiv")
}

\arguments{
\item{object}{an object of class 'morphodata'.}
\item{crossval}{crossvalidation mode, sets individual ("\code{indiv}"; default) or whole populations ("\code{pop}") as leave-out unit.}
\item{k}{number of neighbours considered.}
}


\value{
object of class '\code{\link{classifdata}}'.
}




\details{
The \code{classif.lda} performs classification using linear discriminant function with cross-validation using the \code{lda} function from the package \code{MASS}. Nonparametric classification method k-nearest neighbours is performed using the \code{knn} and \code{knn.cv} functions from the package \code{class}.

The mode of crossvalidation is set by the parameter \code{crossval}. The default "\code{indiv}" uses the standard one-leave-out method. However, as some hierarchical structure is usually present in the data (individuals from a population are not completely independent observations, as they are closer to each other than to individuals from other populations), the value "\code{pop}" sets whole populations as leave-out units. The latter method does not allow classification if there is only one population for a taxon and is more sensitive to “atypical” populations, which usually leads to a somewhat lower classification success rate.
}


\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

# classification by linear discriminant function
classifRes = classif.lda(centaurea, crossval = "indiv")


# classification by nonparametric k-nearest neighbour method

knn.select(centaurea, crossval = "pop") # The optimal K is: 12
classif.knn(centaurea, 12, crossval = "pop")

}
