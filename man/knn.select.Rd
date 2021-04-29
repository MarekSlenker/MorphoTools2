\name{knn.select}
\alias{knn.select}

\title{Search for the Optimal K-nearest Neighbours}


\description{
This function search for the optimal number of neighbours for the given data set for k-nearest neighbour cross-validatory classification.
}


\usage{
knn.select(object, crossval = "indiv")
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}
\item{crossval}{crossvalidation mode, sets individual (\code{"indiv"}; default, one-leave-out method) or whole populations (\code{"pop"}) as leave-out unit.}
}



\details{
The \code{knn.select} function compute number of correctly classified individuals for k values ranging from 1 to 30 and highlight the value with the highest success rate. Ties (i.e., when there are the same numbers of votes for two or more groups) are broken at random, and thus several iterations may yield different results. Therefore, the functions compute 10 iterations, and the average success rates for each k are used; the minimum and maximum success rates for each k are also displayed as error bars. Note that several k values may have nearly the same success rates; if this is the case, the similarity of iterations may also be considered.

The mode of crossvalidation is set by the parameter \code{crossval}. The default \code{"indiv"} uses the standard one-leave-out method. However, as some hierarchical structure is usually present in the data (individuals from a population are not completely independent observations, as they are morphologically closer to each other than to individuals from other populations), the value \code{"pop"} sets whole populations as leave-out units. The latter method does not allow classification if there is only one population for a taxon and is more sensitive to “atypical” populations, which usually leads to a somewhat lower classification success rate.
}

\seealso{
\code{\link{classif.lda}}, \code{\link{classif.knn}}
}



\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))


# classification by nonparametric k-nearest neighbour method
knn.select(centaurea, crossval = "indiv")
classifRes.knn = classif.knn(centaurea, k = 12, crossval = "indiv")

}
