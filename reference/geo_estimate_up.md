# Aggregate Values Up to a Larger Geography

Aggregate values from a smaller geography to a larger geography after
matching each row of `from` to a row of `to`. This is commonly used to
roll block-level counts up to precincts or districts.

## Usage

``` r
geo_estimate_up(from, to, value, method = "center", epsg = 3857)
```

## Arguments

- from:

  Smaller geography level.

- to:

  Larger geography level.

- value:

  Numeric vector of length `nrow(from)` to aggregate. Defaults to `1`.

- method:

  Matching method passed to
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md).

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

numeric vector of length `nrow(to)` with values aggregated by group

## Details

Groups in `to` with no matched rows are included in the output and
receive `0`.

## Examples

``` r
library(dplyr)
set.seed(1)
data(checkerboard)
counties <- checkerboard |>
  group_by(id <= 32) |>
  summarize(geometry = sf::st_union(geometry)) |>
  mutate(pop = c(100, 200))
geo_estimate_up(from = checkerboard, to = counties, value = checkerboard$i)
#> Warning: Planarizing skipped. `x` missing CRS.
#> Warning: Planarizing skipped. `x` missing CRS.
#> [1] 176  48
```
