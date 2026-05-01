# Compute Local Moran's I

Compute Local Moran's I for each observation using either an `sf`
object, a zero-indexed adjacency list, or a spatial weights matrix.

## Usage

``` r
local_morans(shp, adj, wts, spatial_mat, epsg = 3857)
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

tibble with columns `moran`, `expectation`, and `variance`

## Examples

``` r
library(dplyr)
data('checkerboard')
checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) %% 2 == 0))
local_morans(shp = checkerboard, wts = checkerboard$m)
#> Warning: Planarizing skipped. `x` missing CRS.
#> # A tibble: 64 × 3
#>    moran expectation variance
#>    <dbl>       <dbl>    <dbl>
#>  1    -2     -0.0317     1.97
#>  2    -3     -0.0476     2.90
#>  3    -3     -0.0476     2.90
#>  4    -3     -0.0476     2.90
#>  5    -3     -0.0476     2.90
#>  6    -3     -0.0476     2.90
#>  7    -3     -0.0476     2.90
#>  8    -2     -0.0317     1.97
#>  9    -3     -0.0476     2.90
#> 10    -4     -0.0635     3.81
#> # ℹ 54 more rows
```
