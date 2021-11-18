\name{classif.lda}
\alias{classif.lda}
\alias{classif.qda}
\alias{classif.knn}

\title{Classificatory Discriminant Analysis}

\description{
These functions computes discriminant function for classifying observations. Linear discriminant function (\code{classif.lda}), quadratic discriminant function (\code{classif.qda}), or nonparametric k-nearest neighbours classification method (\code{classif.knn}) can be used.
}

\usage{
classif.lda(object, crossval = "indiv")

classif.qda(object, crossval = "indiv")

classif.knn(object, k, crossval = "indiv")
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{crossval}{crossvalidation mode, sets individual (\code{"indiv"}; default, one-leave-out method) or whole populations (\code{"pop"}) as leave-out unit.}
\item{k}{number of neighbours considered for the k-nearest neighbours method.}
}

\value{
an object of class \code{\link{classifdata}} with the following elements:

\item{ID}{IDs of each row.}
\item{Population}{population membership of each row.}
\item{Taxon}{taxon membership of each row.}
\item{classif}{classification from discriminant analysis.}
\item{prob}{posterior probabilities of classification into each taxon (if calculated by \code{\link{classif.lda}} or \code{\link{classif.qda}}),
or proportion of the votes for the winning class (calculated by \code{\link{classif.knn}})}
\item{correct}{logical, correctness of classification.}
}


\details{
The \code{classif.lda} and \code{classif.qda} performs classification using linear and quadratic discriminant functions with cross-validation using the \code{lda} and \code{qda} functions from the package \code{MASS}. The prior probabilities of group memberships are equal.

LDA and QDA analyses have some requirements: (1) no character can be a linear combination of any other character; (2) no pair of characters can be highly correlated; (3) no character can be invariant in any taxon; (4) for the number of taxa (g), characters (p) and total number of samples (n) should hold: 0 \eqn{<} p \eqn{<} (n - g), and (5) there must be at least two groups (taxa), and in each group there must be at least two objects. Violation of some of these assumptions may result in warnings or error messages (rank deficiency).

Nonparametric classification method k-nearest neighbours is performed using the \code{knn} and \code{knn.cv} functions from the package \code{class}.

The mode of crossvalidation is set by the parameter \code{crossval}. The default \code{"indiv"} uses the standard one-leave-out method. However, as some hierarchical structure is usually present in the data (individuals from a population are not completely independent observations, as they are morphologically closer to each other than to individuals from other populations), the value \code{"pop"} sets whole populations as leave-out units. The latter method does not allow classification if there is only one population for a taxon and is more sensitive to ``atypical'' populations, which usually leads to a somewhat lower classification success rate.
}

\seealso{
\code{\link{classifSample.lda}},
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



# classification by linear discriminant function
classifRes.lda = classif.lda(centaurea, crossval = "indiv")

# classification by quadratic discriminant function
classifRes.qda = classif.qda(centaurea, crossval = "indiv")

# classification by nonparametric k-nearest neighbour method
# use knn.select to find the optimal K.
\dontrun{knn.select(centaurea, crossval = "pop")}
classifRes.knn = classif.knn(centaurea, k = 12, crossval = "pop")

# exporting results
classif.matrix(classifRes.lda, level = "taxon")
classif.matrix(classifRes.qda, level = "taxon")
classif.matrix(classifRes.knn, level = "taxon")

}
