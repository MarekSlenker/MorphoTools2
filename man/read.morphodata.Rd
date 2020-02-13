\name{read.morphodata}
\alias{read.morphodata}
\alias{summary.morphodata}
\alias{samples}
\alias{populations}
\alias{taxa}

\title{Data input & description}
\usage{
read.morphodata(file, dec=".", sep = "\t", ...)

## S3 methods for class 'morphodata'
summary(object)

samples(object)

populations(object)

taxa(object)

}
\arguments{
\item{file}{the name of the file which the data are to be read from.}

\item{object}{an object of class 'morphodata'.}

\item{dec}{the character used in the file for decimal points.}

\item{sep}{the column separator character.}

\item{...}{further arguments to be passed to read.table.}
}
\value{
object of class '\code{\link{morphodata}}'
}
\description{
Imports data from text file and produces a 'morphodata' object from it.
}
\details{
The function expect the following data structure: \cr
(1) the first row contains variable names;\cr
(2) the following rows contains individuals, single individual per row;\cr
(3) the first three columns include unique identifiers for individuals, populations and taxa/groups, respectively. Columns have to be named as “ID”, “Population” and “Taxon”;\cr
(4) starting from the fourth column, any number of quantitative or binary morphological characters may be recorded. Any column names can be used (avoiding spaces and special characters);

If there are missing values in the data, they must be represented as empty cells or by the text NA (not quoted), not zero, space or any other character.
}
\examples{
data = read.morphodata("infile.txt")
data = read.morphodata("clipboard")

summary(data)
samples(data)
populations(data)
taxa(data)
}
