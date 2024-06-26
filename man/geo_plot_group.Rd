% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{geo_plot_group}
\alias{geo_plot_group}
\title{Create Plots of Shapes by Group with Connected Components Colored}
\usage{
geo_plot_group(shp, adj, group, save = FALSE, path = "")
}
\arguments{
\item{shp}{An sf shapefile}

\item{adj}{adjacency list}

\item{group}{array of group identifiers. Typically district numbers or county names.}

\item{save}{Boolean, whether to save or not.}

\item{path}{Path to save, only used if save is TRUE. Defaults to working directory.}
}
\value{
list of ggplots
}
\description{
Create Plots of Shapes by Group with Connected Components Colored
}
\examples{
library(dplyr)
data('checkerboard')
data('checkerboard_adj')

checkerboard <- checkerboard |> mutate(discont = as.integer(j == 5 | j == 6))

p <- geo_plot_group(checkerboard, checkerboard_adj, checkerboard$discont)

p[[1]]
p[[2]]
}
\concept{plot}
