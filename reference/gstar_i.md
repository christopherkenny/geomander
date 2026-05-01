# Compute Standardized Getis-Ord G\*i

Returns standardized local Getis-Ord G\*i scores using either an `sf`
object, a zero-indexed adjacency list, or a spatial weights matrix.

## Usage

``` r
gstar_i(shp, adj, wts, spatial_mat, epsg = 3857)
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

numeric vector of standardized G\*i scores, one per observation

## Examples

``` r
library(dplyr)
data('checkerboard')
checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) %% 2 == 0))
gstar_i(shp = checkerboard, wts = checkerboard$m)
#> Warning: Planarizing skipped. `x` missing CRS.
#>  [1]  1.425573 -1.760216  1.760216 -1.760216  1.760216 -1.760216  1.760216
#>  [8] -1.425573 -1.760216  2.049390 -2.049390  2.049390 -2.049390  2.049390
#> [15] -2.049390  1.760216  1.760216 -2.049390  2.049390 -2.049390  2.049390
#> [22] -2.049390  2.049390 -1.760216 -1.760216  2.049390 -2.049390  2.049390
#> [29] -2.049390  2.049390 -2.049390  1.760216  1.760216 -2.049390  2.049390
#> [36] -2.049390  2.049390 -2.049390  2.049390 -1.760216 -1.760216  2.049390
#> [43] -2.049390  2.049390 -2.049390  2.049390 -2.049390  1.760216  1.760216
#> [50] -2.049390  2.049390 -2.049390  2.049390 -2.049390  2.049390 -1.760216
#> [57] -1.425573  1.760216 -1.760216  1.760216 -1.760216  1.760216 -1.760216
#> [64]  1.425573
```
