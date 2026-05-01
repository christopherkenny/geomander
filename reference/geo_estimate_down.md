# Estimate Values Down to a Finer Geography

Split values observed on a larger geography across a smaller geography
after first matching each row of `to` to a row of `from`. This is
commonly used to distribute precinct-level election totals to blocks.

## Usage

``` r
geo_estimate_down(from, to, wts, value, method = "center", epsg = 3857)
```

## Arguments

- from:

  Larger geography level containing the observed values.

- to:

  Smaller geography level to estimate onto.

- wts:

  Numeric vector of length `nrow(to)`. Used to allocate each matched
  value across rows of `to`. Defaults to `1`, which splits evenly within
  each matched group.

- value:

  Numeric vector of length `nrow(from)` containing the values to split
  downward. Defaults to `1`.

- method:

  Matching method passed to
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md).

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

numeric vector of length `nrow(to)` with estimated values

## Details

If all weights for a matched group are zero, the function falls back to
equal allocation within that group. Rows in `to` that do not match any
row of `from` receive `0`.

## Examples

``` r
library(dplyr)
set.seed(1)
data(checkerboard)
counties <- checkerboard |>
  group_by(id <= 32) |>
  summarize(geometry = sf::st_union(geometry)) |>
  mutate(pop = c(100, 200))
geo_estimate_down(from = counties, to = checkerboard, value = counties$pop)
#> Warning: Planarizing skipped. `x` missing CRS.
#> Warning: Planarizing skipped. `x` missing CRS.
#>  [1] 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250
#> [13] 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250
#> [25] 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 3.125 3.125 3.125 3.125
#> [37] 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125
#> [49] 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125
#> [61] 3.125 3.125 3.125 3.125
```
