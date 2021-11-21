\name{exportRes}
\alias{exportRes}

\title{Export Data}

\description{
This function is designed for exporting results, stored in objects of \code{MorphoTools2} package.
}

\usage{
exportRes(object, file = "", dec = ".", sep = "\\t",
          row.names = FALSE, col.names = TRUE)
}

\arguments{
\item{object}{an object to be exported.}
\item{file}{either a character string naming a file or a \code{\link{connection}} opened for writing (e.g., \code{"clipboard"}). \code{""} indicates output to the console.}
\item{dec}{the character used for decimal points.}
\item{sep}{the column separator character.}
\item{row.names}{logical, if \code{TRUE}, row names of the \code{object} are to be written.}
\item{col.names}{logical, if \code{TRUE}, column names of the \code{object} are to be written.}
}

\value{
None. Used for its side effect.
}

\examples{
data(centaurea)

descr = descrTaxon(centaurea, format = "($MEAN ± $SD)")
\donttest{exportRes(descr, file = "centaurea_descrTax.txt")}
}


