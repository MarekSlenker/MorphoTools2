\name{descr.tax}
\alias{descr.tax}
\alias{descr.pop}
\alias{descr.all}

\title{Descriptive statistics}

\usage{
descr.tax(object, format)

descr.pop(object, format)

descr.all(object, format)
}
\arguments{
\item{object}{an object of class 'morphodata'.}

\item{format}{form to which will be formatted descriptive characters. See Details.}
}
\description{
These functions calculate the descriptive statistics of each character in the whole dataset, each taxon and each population.
}
\details{
The following statistics are computed: number of observations, mean, standard deviation, and the percentiles 0%
 (minimum), 5%, 25% (lower quartile), 50% (median), 75% (upper quartile), 95% and 100% (maximum).

The \code{format} argument brings a handy way how to receive only what is wanted and in format what is desired. 
Otherways, if format remains NULL, output table is overwhelmed by all calculated descriptors. 
The format argument is a single string, where keywords will be replaced by particular values.

Keywords: "$MEAN" = mean; "$SD" = standard deviation"; "$MIN" = minimum; "$5\%" =  5th percentile;
"$25\%" = 25th percentile (lower quartile); "$MEDIAN" = median (50th percentile); "$75\%" = 75th percentile (upper quartile);
"$95\%" = 95th percentile; "$MAX" = maximum.
}

\examples{
data(centaurea)

descr.tax(centaurea, format = "($MEAN ± $SD)")

descr.pop(centaurea, format = "$MEAN ($MIN - $MAX)")

descr.all(centaurea, format = "$MEAN ± $SD ($5\% - $95\%)")
}
