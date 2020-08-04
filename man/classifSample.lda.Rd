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

classifSample.knn(sampleData, trainingData, k)
}


\arguments{
\item{sampleData}{observations which should be classified. An object of class 'morphodata'.}
\item{trainingData}{observations for computing discriminant function. An object of class 'morphodata'.}
\item{k}{number of neighbours considered.}
}

\value{
object of class '\code{\link{classifdata}}'.
}



\details{
The \code{classifSample.lda} performs classification using linear discriminant function with cross-validation using the \code{lda} function from the package \code{MASS}. Nonparametric classification method k-nearest neighbours is performed using the \code{knn} functions from the package \code{class}.

The mode of crossvalidation is the standard one-leave-out method ("\code{indiv}").

The \code{classifSample} methods allow you to store sample data and training data in separate objects. 
But whether you do it this way or you have a hybrid population or sample along with training observations
 as part of a single object and use the \code{classif.lda} or \code{classif.knn} with
 \code{crossval = "pop"}, the result is the same.
}


\examples{
data(centaurea)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))

trainingSet = deletePopulation(centaurea, populationName = "SOK")
SOK = keepPopulation(centaurea, populationName = "SOK")


# classification by linear discriminant function
classif.lda.SOK = classifSample.lda(SOK, trainingSet)


# classification by nonparametric k-nearest neighbour method
knn.select(trainingSet, crossval = "pop") # The optimal K is: 15
classif.knn.SOK = classifSample.knn(SOK, trainingSet, k = 15)
}



