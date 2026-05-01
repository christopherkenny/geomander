# Estimate Values Down Using Precomputed Matches

Non-geographic companion to
[`geo_estimate_down()`](https://christophertkenny.com/geomander/reference/geo_estimate_down.md).
Use this when you already have a vector of matches and want to avoid
recomputing them.

## Usage

``` r
estimate_down(wts, value, group)
```

## Arguments

- wts:

  Numeric vector of weights. Defaults to `1`.

- value:

  Numeric vector of values on the larger geography. Defaults to `1`.

- group:

  Integer vector of matches of length `length(wts)`. Each entry should
  give the row of `value` that the corresponding lower-level unit maps
  to, often from
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md).

## Value

numeric vector of length `length(group)` with values split by weight

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
set.seed(1)
data(checkerboard)
counties <- checkerboard |>
  group_by(id <= 32) |>
  summarize(geometry = sf::st_union(geometry)) |>
  mutate(pop = c(100, 200))
matches <- geo_match(checkerboard, counties)
#> Warning: Planarizing skipped. `x` missing CRS.
#> Warning: Planarizing skipped. `x` missing CRS.
estimate_down(wts = rep(1, nrow(checkerboard)), value = counties$pop, group = matches)
#>  [1] 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250
#> [13] 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250
#> [25] 6.250 6.250 6.250 6.250 6.250 6.250 6.250 6.250 3.125 3.125 3.125 3.125
#> [37] 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125
#> [49] 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125 3.125
#> [61] 3.125 3.125 3.125 3.125
```
