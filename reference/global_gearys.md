# Compute Global Geary's C

Computes the Global Geary's Contiguity statistic. Can produce spatial
weights from an adjacency or sf dataframe, in which case the spatial_mat
is a contiguity matrix. Users can also provide a spatial_mat argument
directly.

## Usage

``` r
global_gearys(shp, adj, wts, spatial_mat, epsg = 3857)
```

## Arguments

- shp:

  `sf` dataframe. Optional if `adj` or `spatial_mat` is supplied.

- adj:

  Zero-indexed adjacency list. Optional if `shp` or `spatial_mat` is
  supplied.

- wts:

  Numeric vector of observed values.

- spatial_mat:

  Square spatial weights matrix. Optional if `shp` or `adj` is supplied.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

numeric scalar containing the global Geary's C statistic

## Examples

``` r
library(dplyr)
data('checkerboard')
checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) %% 2 == 0))
global_gearys(shp = checkerboard, wts = checkerboard$m)
#> Warning: Planarizing skipped. `x` missing CRS.
#> [1] 1.96875
```
