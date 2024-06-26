% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{geo_estimate_up}
\alias{geo_estimate_up}
\title{Estimate Up Geography Levels}
\usage{
geo_estimate_up(from, to, value, method = "center", epsg = 3857)
}
\arguments{
\item{from}{smaller geography level}

\item{to}{larger geography level}

\item{value}{numeric vector of length nrow(from). Defaults to 1.}

\item{method}{string from center, centroid, point, or area for matching levels}

\item{epsg}{numeric EPSG code to planarize to. Default is 3857.}
}
\value{
numeric vector with each value aggregated by group
}
\description{
Simple method for aggregating data up to a higher level This is most often useful
for getting population data from a block level up to a precinct level.
Geographic partner to estimate_up.
}
\examples{
library(dplyr)
set.seed(1)
data(checkerboard)
counties <- checkerboard |>
  group_by(id <= 32) |>
  summarize(geometry = sf::st_union(geometry)) |>
  mutate(pop = c(100, 200))
geo_estimate_up(from = checkerboard, to = counties, value = checkerboard$i)
}
\concept{estimate}
