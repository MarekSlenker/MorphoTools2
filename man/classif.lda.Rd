\name{classif.lda}
\alias{classif.lda}
\alias{classif.knn}
\alias{classif.matrix}

\title{Classificatory Discriminant Analysis}


\description{
These functions computes discriminant function for classifying observations. Linear discriminant function (\code{classif.lda}) or nonparametric k-nearest neighbours classification method (\code{classif.knn}) can be used.
}


\usage{
classif.lda(object, crossval = "indiv")

classif.knn(object, k, crossval = "indiv")

classif.matrix(result, level = "taxon")
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
\item{result}{an object of class '\code{\link{classifdata}}'.}
\item{crossval}{crossvalidation mode, sets individual (\code{"indiv"}; default, one-leave-out method) or whole populations (\code{"pop"}) as leave-out unit.}
\item{k}{number of neighbours considered for the k-nearest neighbours method.}
\item{level}{level of grouping of classification matrix, \code{"taxon"} (default), populations (\code{"pop"}), or individuals (\code{"indiv"})}
}


\value{
an object of class '\code{\link{classifdata}}'.
}


\details{
The \code{classif.lda} performs classification using linear discriminant function with cross-validation using the \code{lda} function from the package \code{MASS}. The prior probabilities of group memberships are equal.

Nonparametric classification method k-nearest neighbours is performed using the \code{knn} and \code{knn.cv} functions from the package \code{class}.

The mode of crossvalidation is set by the parameter \code{crossval}. The default \code{"indiv"} uses the standard one-leave-out method. However, as some hierarchical structure is usually present in the data (individuals from a population are not completely independent observations, as they are morphologically closer to each other than to individuals from other populations), the value \code{"pop"} sets whole populations as leave-out units. The latter method does not allow classification if there is only one population for a taxon and is more sensitive to “atypical” populations, which usually leads to a somewhat lower classification success rate.

The \code{classif.matrix} method formats the results of the above functions as a summary classification table of taxa, populations, or individuals.

}

\seealso{
\code{\link{knn.select}}
}

\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

# classification by linear discriminant function
classifRes.lda = classif.lda(centaurea, crossval = "indiv")

# classification by nonparametric k-nearest neighbour method
# USE knn.select(centaurea, crossval = "pop") to find the optimal K.
classifRes.knn = classif.knn(centaurea, k = 12, crossval = "pop")

# exporting results
classif.matrix(classifRes.lda, level = "taxon")
classif.matrix(classifRes.knn, level = "taxon")
}
