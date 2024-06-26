% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moran.R
\name{global_morans}
\alias{global_morans}
\title{Compute Global Moran's I}
\usage{
global_morans(shp, adj, wts, spatial_mat, epsg = 3857)
}
\arguments{
\item{shp}{sf data frame. Optional if adj or spatial_mat provided.}

\item{adj}{zero indexed adjacency list. Optional if shp or spatial_mat provided.}

\item{wts}{Required. Numeric vector with weights to use for Moran's I.}

\item{spatial_mat}{matrix of spatial weights. Optional if shp or adj provided.}

\item{epsg}{numeric EPSG code to planarize to. Default is 3857.}
}
\value{
list
}
\description{
Computes the Global Moran's I statistic and expectation. Can produce spatial weights
from an adjacency or sf data frame, in which case the spatial_mat is a contiguity
matrix. Users can also provide a spatial_mat argument directly.
}
\examples{
library(dplyr)
data('checkerboard')
checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) \%\% 2 == 0))
global_morans(shp = checkerboard, wts = checkerboard$m)
}
\concept{spatcorr}
