\name{export.res}
\alias{export.res}


\title{Export data}

\description{
This function is designed for exporting results, stored in objects of MorphoTools package.
}


\usage{
export.res(object, file = "clipboard", dec= ".", sep = "\\t",
          row.names = FALSE, col.names = TRUE)
}

\arguments{
\item{object}{an object to be exported.}

\item{file}{either a character string naming a file or a \code{\link{connection}} open for writing. "" indicates output to the console.}

\item{dec}{the string to use for decimal points in numeric or complex columns: must be a single character.}

\item{sep}{the column separator character.}

\item{row.names}{logical, if \code{TRUE}, row names of the \code{object} are to be written.}

\item{col.names}{logical, if \code{TRUE}, column names of the \code{object} are to be written.}
}

\examples{
data(centaurea)

descr = descr.tax(centaurea, format = "($MEAN ± $SD)")
export.res(descr, file = "centaurea_descrTax.txt")
}


