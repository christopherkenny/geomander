# Compute Local Geary's C

Compute Local Geary's C for each observation using either an `sf`
object, a zero-indexed adjacency list, or a spatial weights matrix.

## Usage

``` r
local_gearys(shp, adj, wts, spatial_mat, epsg = 3857)
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

tibble with one column, `geary`, and one row per observation

## Examples

``` r
library(dplyr)
data('checkerboard')
checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) %% 2 == 0))
local_gearys(shp = checkerboard, wts = checkerboard$m)
#> Warning: Planarizing skipped. `x` missing CRS.
#> # A tibble: 64 × 1
#>    geary
#>    <dbl>
#>  1     8
#>  2    12
#>  3    12
#>  4    12
#>  5    12
#>  6    12
#>  7    12
#>  8     8
#>  9    12
#> 10    16
#> # ℹ 54 more rows
```
