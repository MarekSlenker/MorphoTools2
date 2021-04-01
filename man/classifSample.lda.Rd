\name{classifSample.lda}
\alias{classifSample.knn}
\alias{classifSample.lda}

\title{Classificatory discriminant analysis}

\description{
These functions compute discriminant function based on an independent training set and classify observations in sample set.
Linear discriminant function (\code{classif.lda}) or nonparametric k-nearest neighbor classification method (\code{classif.knn}) can be used.
}

\usage{
classifSample.lda(sampleData, trainingData)

classifSample.knn(sampleData, trainingData, k)
}


\arguments{
\item{sampleData}{observations which should be classified. An object of class '\code{\link{morphodata}}'.}
\item{trainingData}{observations for computing discriminant function. An object of class '\code{\link{morphodata}}'.}
\item{k}{number of neighbours considered.}
}

\value{
an object of class '\code{\link{classifdata}}'.
}



\details{
The \code{classifSample.lda} performs classification using linear discriminant function using the \code{lda} function from the package \code{MASS}. Nonparametric classification method \code{classifSample.knn} (k-nearest neighbours) is performed using the \code{knn} functions from the package \code{class}.

The \code{classifSample} functions are designed to classify hybrid populations, type herbarium specimens, atypical samples, entirely new data, etc. Discriminant criterion is developed from the original (training) dataset and applied to the specific sample (set).
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
# USE knn.select(trainingSet, crossval = "pop") to find the optimal K.
classif.knn.SOK = classifSample.knn(SOK, trainingSet, k = 15)
}



