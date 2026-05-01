# Compute Global Moran's I

Computes the Global Moran's I statistic and expectation. Can produce
spatial weights from an adjacency or sf dataframe, in which case the
spatial_mat is a contiguity matrix. Users can also provide a spatial_mat
argument directly.

## Usage

``` r
global_morans(shp, adj, wts, spatial_mat, epsg = 3857)
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

named list returned by the underlying C++ implementation, including the
global statistic and its moments.

## Examples

``` r
library(dplyr)
data('checkerboard')
checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) %% 2 == 0))
global_morans(shp = checkerboard, wts = checkerboard$m)
#> Warning: Planarizing skipped. `x` missing CRS.
#> $moran
#> [1] -1
#> 
#> $expectation
#> [1] -0.01587302
#> 
```
