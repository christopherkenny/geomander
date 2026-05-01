# Aggregate Values Up Using Precomputed Matches

Non-geographic companion to
[`geo_estimate_up()`](https://christophertkenny.com/geomander/reference/geo_estimate_up.md).
Use this when you already have a vector of group assignments.

## Usage

``` r
estimate_up(value, group)
```

## Arguments

- value:

  Numeric vector to aggregate. Defaults to `1`.

- group:

  Integer vector of length `length(value)` giving the destination row
  for each value, often from
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md).

## Value

numeric vector of length `max(group)` with values aggregated by group

## Examples

``` r
library(dplyr)
set.seed(1)
data(checkerboard)
counties <- checkerboard |>
  group_by(id <= 32) |>
  summarize(geometry = sf::st_union(geometry)) |>
  mutate(pop = c(100, 200))
matches <- geo_match(checkerboard, counties)
#> Warning: Planarizing skipped. `x` missing CRS.
#> Warning: Planarizing skipped. `x` missing CRS.
estimate_up(value = checkerboard$i, group = matches)
#> [1] 176  48
```
