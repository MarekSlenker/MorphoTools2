\name{classifSample.lda}
\alias{classifSample.knn}
\alias{classifSample.lda}
\alias{classifSample.qda}

\title{Classificatory Discriminant Analysis}

\description{
These functions compute discriminant function based on an independent training set and classify observations in sample set.
Linear discriminant function (\code{classifSample.lda}), quadratic discriminant function (\code{classifSample.qda}), or nonparametric k-nearest neighbour classification method (\code{classifSample.knn}) can be used.
}

\usage{
classifSample.lda(sampleData, trainingData)

classifSample.qda(sampleData, trainingData)

classifSample.knn(sampleData, trainingData, k)
}


\arguments{
\item{sampleData}{observations which should be classified. An object of class \code{\link{morphodata}}.}
\item{trainingData}{observations for computing discriminant function. An object of class \code{\link{morphodata}}.}
\item{k}{number of neighbours considered.}
}

\value{
an object of class \code{\link{classifdata}}.
}



\details{
The \code{classifSample.lda} and \code{classifSample.qda} performs classification using linear and quadratic discriminant function using the \code{lda} and \code{qda} functions from the package \code{MASS}. Nonparametric classification method \code{classifSample.knn} (k-nearest neighbours) is performed using the \code{knn} functions from the package \code{class}. The \code{classifSample} functions are designed to classify hybrid populations, type herbarium specimens, atypical samples, entirely new data, etc. Discriminant criterion is developed from the original (training) dataset and applied to the specific sample (set).

LDA and QDA analyses have some requirements: (1) no character can be a linear combination of any other character; (2) no pair of characters can be highly correlated; (3) no character can be invariant in any taxon (group); (4) for the number of taxa (g), characters (p) and total number of samples (n) should hold: 0 \eqn{<} p \eqn{<} (n - g), and (5) there must be at least two groups (taxa), and in each group there must be at least two objects. Violation of some of these assumptions may result in warnings or error messages (rank deficiency).

}


\seealso{
\code{\link{classif.lda}},
\code{\link{classif.matrix}},
\code{\link{knn.select}}
}

\examples{
data(centaurea)

# remove NAs and linearly dependent characters (characters with unique contributions
#                  can be identified by stepwise discriminant analysis.)
centaurea = naMeanSubst(centaurea)
centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))
centaurea = keepCharacter(centaurea, c("MLW", "ML", "IW", "LS", "IV", "MW", "MF",
                                    "AP", "IS", "LBA", "LW", "AL", "ILW", "LBS",
                                    "SFT", "CG", "IL", "LM", "ALW", "AW", "SF") )
# add a small constant to characters witch are invariant within taxa
centaurea$data[ centaurea$Taxon == "hybr", "LM" ][1] =
             centaurea$data[ centaurea$Taxon == "hybr", "LM" ][1] + 0.000001
centaurea$data[ centaurea$Taxon == "ph", "IV" ][1] =
             centaurea$data[ centaurea$Taxon == "ph", "IV" ][1] + 0.000001
centaurea$data[ centaurea$Taxon == "st", "LBS"][1] =
             centaurea$data[ centaurea$Taxon == "st", "LBS"][1] + 0.000001

#

trainingSet = deletePopulation(centaurea, populationName = "LES")
LES = keepPopulation(centaurea, populationName = "LES")


# classification by linear discriminant function
classifSample.lda(LES, trainingSet)

# classification by quadratic discriminant function
classifSample.qda(LES, trainingSet)

# classification by nonparametric k-nearest neighbour method
# USE knn.select(trainingSet) to find the optimal K.
classifSample.knn(LES, trainingSet, k = 12)
}



