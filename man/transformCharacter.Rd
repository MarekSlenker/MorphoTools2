\name{transformCharacter}
\alias{transformCharacter}
\title{Transformation of character.}

\usage{
transformCharacter(object, character, FUN, newName = NULL)
}

\arguments{
\item{object}{an object of class '\code{\link{morphodata}}'.}

\item{character}{a morphological character that should be transformed.}

\item{FUN}{the transforming function to be applied to character.}

\item{newName}{a name to rename the original character. If \code{NULL}, the name of the transformed character remains the same.}
}

\description{
Transformation is applyed to characters to improve their distribution (to become normally distributed or at least to achieve less deviation from normality). The \code{FUN} argument takes any function. In the examples bellow, we used a number 1 as constant, but note, the chosen constant affect the result. See MorphoTools2 tutorial for details.
}

\examples{
data(centaurea)

# For a right-skewed (positive) distribution can be used:
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) log(x+1))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) log10(x+1))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) sqrt(x))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) xˆ(1/3))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) asin(sqrt(x)))

# For a left-skewed (negative) distribution can be used:
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) log((max(x)+1)-x))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) log10((max(x)+1)-x))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) sqrt((max(x)+1)-x))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) ((max(x)+1)-x)ˆ(1/3))
cTransf = transformCharacter(centaurea, character = "SF", FUN = function(x) asin(sqrt((max(x))-x)))


}
