\name{read.morphodata}
\alias{read.morphodata}
\alias{samples}
\alias{populations}
\alias{taxa}

\title{Data Input and Description}

\description{
This function imports data and produces a \code{\link{morphodata}} object from it.
}

\usage{
read.morphodata(file, dec = ".", sep = "\t", ...)

## S3 method for class 'morphodata'

samples(object)

populations(object)

taxa(object)
}

\arguments{
\item{file}{the file which the data are to be read from or a \code{\link{connection}} for reading (e.g., \code{"clipboard"}).}
\item{dec}{the character used for decimal points.}
\item{sep}{the column separator character.}
\item{object}{an object of class \code{\link{morphodata}}.}
\item{...}{further arguments to be passed to \code{\link{read.table}} function.}
}

\value{
an object of class \code{\link{morphodata}} with the following elements:

\item{ID}{IDs of each row of \code{data} object.}
\item{Population}{population membership of each row of \code{data} object.}
\item{Taxon}{taxon membership of each row of \code{data} object.}
\item{data}{\code{data.frame} of individuals (rows) and values of measured morphological characters (columns).}
}

\details{
The function expects the following data structure: \cr
(1) the first row contains variable names;\cr
(2) the following rows contains individuals, single individual per row;\cr
(3) the first three columns include unique identifiers for individuals, populations and taxa/groups, respectively. Columns have to be named as ``ID'', ``Population'' and ``Taxon'';\cr
(4) starting from the fourth column, any number of quantitative or binary morphological characters may be recorded. Any variable names can be used (avoiding spaces and special characters);

If there are missing values in the data, they must be represented as empty cells or by the text \code{NA}, not zero, space or any other character. Example dataset in txt and xlsx formats are stored in the ``extdata'' directory of the MorphoTools2 package installation directory. To find the path to the package location run \code{system.file("extdata", package = "MorphoTools2")}.

}

\seealso{
\code{\link{characters}}
}


\examples{
data = read.morphodata(file = system.file("extdata", "centaurea.txt",
    package = "MorphoTools2"), dec = ".", sep = "\t")

\donttest{data = read.morphodata(file = "morphodata.txt", dec = ".", sep = "\t")}

\donttest{data = read.morphodata("clipboard")}

summary(data)
samples(data)
populations(data)
taxa(data)
}
