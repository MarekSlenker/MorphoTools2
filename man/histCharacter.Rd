\name{histCharacter}
\alias{histCharacter}
\alias{histAll}
\title{Histograms of characters.}
\usage{
histCharacter(
  object,
  character,
  taxon = NULL,
  histogram = TRUE,
  col = "grey90",
  densityLine = TRUE,
  normDistLine = TRUE,
  ...
)

histAll(
  object,
  folderName = "histograms",
  taxon = NULL,
  histogram = TRUE,
  col = "grey90",
  densityLine = TRUE,
  normDistLine = TRUE,
  width = 480,
  height = 480,
  units = "px",
  ...
)
}
\description{
Histograms of characters.
}

\examples{
data(centaurea)

histCharacter(centaurea, character = "IW", breaks = seq(0.5, 2.5, 0.1))


}