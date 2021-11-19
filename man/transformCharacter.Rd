\name{transformCharacter}
\alias{transformCharacter}
\title{Transformation of Character}

\description{
This function transforms morphological characters by applying another function passed in the argument.
}

\usage{
transformCharacter(object, character, FUN, newName = NULL)
}

\arguments{
\item{object}{an object of class \code{\link{morphodata}}.}
\item{character}{a morphological character that should be transformed.}
\item{FUN}{the transforming function to be applied to character.}
\item{newName}{a name to rename the original character. If \code{NULL}, the name of the transformed character remains the same.}
}

\value{
an object of class \code{\link{morphodata}} with the following elements:

\item{ID}{IDs of each row of \code{data} object.}
\item{Population}{population membership of each row of \code{data} object.}
\item{Taxon}{taxon membership of each row of \code{data} object.}
\item{data}{\code{data.frame} of individuals (rows) and values of measured morphological characters (columns).}
}

\details{
Transformation is applied to characters to improve their distribution (to become normally distributed or at least to achieve lesser deviation from normality). The \code{FUN} argument takes any function, able to accept as input any value of the character specified by \code{character} argument.

Note that, when using a log transformation, a constant should be added to all values to make them all positive before transformation (if there are zero values in the data), because the argument of the logarithm can be only positive numbers. The arcsine transformation is applicable for proportions and percentages (for values ranging from 0 to 1).
}

\examples{
data(centaurea)

# For a right-skewed (positive) distribution can be used:
# Logarithmic transformation
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) log(x+1))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) log10(x+1))
# Square root transformation
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) sqrt(x))
# Cube root transformation
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) x^(1/3))
# Arcsine transformation
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) asin(sqrt(x)))

# For a left-skewed (negative) distribution can be used:
# Logarithmic transformation
cTransf = transformCharacter(centaurea, character="SF", FUN=function(x) log((max(x)+1)-x))
cTransf = transformCharacter(centaurea, character="SF", FUN=function(x) log10((max(x)+1)-x))
# Square root transformation
cTransf = transformCharacter(centaurea, character="SF", FUN=function(x) sqrt((max(x)+1)-x))
# Cube root transformation
cTransf = transformCharacter(centaurea, character="SF", FUN=function(x) ((max(x)+1)-x)^(1/3))
# Arcsine transformation
cTransf = transformCharacter(centaurea, character="SF", FUN=function(x) asin(sqrt((max(x))-x)))


}
