\name{classifSample.knn}
\alias{classifSample.knn}
\alias{classifSample.lda}

\title{Classificatory discriminant analysis}

\description{
These functions computes discriminant function based on an independent training set and classify observations in sample set.
Linear discriminant function (\code{classif.lda}) or nonparametric k-nearest-neighbor classification method (\code{classif.knn}) can be used.
}

\usage{
classifSample.lda(sampleData, trainingData)

knn.select(trainingData, crossval = "indiv")
classifSample.knn(sampleData, trainingData, k)
}


\arguments{
\item{sampleData}{observations which should be classified. An object of class 'morphodata'.}
\item{trainingData}{observations for computing discriminant function. An object of class 'morphodata'.}
\item{crossval}{crossvalidation mode, sets individual ("\code{indiv}"; default) or whole populations ("\code{pop}") as leave-out unit.}
\item{k}{number of neighbours considered.}
}

\value{
object of class '\code{\link{classifdata}}'.
}



For each row of the sample set, the k nearest (in Euclidean distance) training set vectors are found, and the classification is decided by majority vote, with ties broken at random. If there are ties for the kth nearest vector, all candidates are included in the vote.



\details{
The \code{classifSample.lda} performs classification using linear discriminant function with cross-validation using the \code{lda} function from the package \code{MASS}. Nonparametric classification method k-nearest neighbours is performed using the \code{knn} functions from the package \code{class}.

The mode of crossvalidation is the standard one-leave-out method ("\code{indiv}").


However, as some hierarchical structure is usually present in the data (individuals from a population are not completely independent observations, as they are closer to each other than to individuals from other populations), the value "\code{pop}" sets whole populations as leave-out units. The latter method does not allow classification if there is only one population for a taxon and is more sensitive to “atypical” populations, which usually leads to a somewhat lower classification success rate.
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



