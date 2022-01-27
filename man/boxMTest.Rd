\name{boxMTest}
\alias{boxMTest}

\title{Box's M-test for Homogeneity of Covariance Matrices}

\description{
The \code{boxMTest} function performs Box's (1949) M-test for homogeneity of covariance matrices. The null hypothesis for this test is that the observed covariance matrices for the dependent variables are equal across groups.
}

\usage{
boxMTest(object)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
}

\value{
None. Used for its side effect.
}

\references{
\strong{Box G.E.P. (1949)}. A general distribution theory for a class of likelihood criteria. \emph{Biometrika} 36, 317-346.
}



\examples{
data(centaurea)

# remove NAs and linearly dependent characters (characters with unique contributions
#                  can be identified by stepwise discriminant analysis.)
centaurea = naMeanSubst(centaurea)
centaurea = removePopulation(centaurea, populationName = c("LIP", "PREL"))
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

boxMTest(centaurea)
}
