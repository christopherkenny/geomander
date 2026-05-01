# Match Features Across Geographic Layers

Match each row of `from` to one row of `to`, typically when `from` is a
finer geography nested inside `to`. The result is an integer vector of
row indices in `to` and can be reused by downstream helpers such as
[`estimate_up()`](https://christophertkenny.com/geomander/reference/estimate_up.md),
[`estimate_down()`](https://christophertkenny.com/geomander/reference/estimate_down.md),
and
[`block2prec()`](https://christophertkenny.com/geomander/reference/block2prec.md).

## Usage

``` r
geo_match(
  from,
  to,
  method = "center",
  by = NULL,
  tiebreaker = TRUE,
  epsg = 3857
)
```

## Arguments

- from:

  An `sf` object to match from, usually the smaller geography.

- to:

  An `sf` object to match to, usually the larger geography.

- method:

  Matching method. One of `"center"`, `"centroid"`, `"point"`,
  `"circle"`, or `"area"`.

- by:

  Optional character scalar restricting matching within shared groups.
  Use a single value when `from` and `to` use the same column name, or a
  named scalar such as `c(county_from = "county_to")` when the column
  names differ.

- tiebreaker:

  Logical. If `TRUE`, ambiguous or unmatched features are assigned to
  the nearest feature in `to`. If `FALSE`, unmatched rows receive `-1`
  and rows with multiple matches receive `-2`.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

integer vector of length `nrow(from)`. Values are row indices in `to`,
or `-1` / `-2` when `tiebreaker = FALSE`. Empty geometries are returned
as `NA`.

## Details

Methods are as follows:

- `"centroid"`: match using the geometric centroid of each row in
  `from`.

- `"center"`: use the centroid when it lies inside the polygon and
  otherwise fall back to a point-on-surface.

- `"point"`: use point on surface for each row in `from`.

- `"circle"`: use the centroid of the maximum inscribed circle.

- `"area"`: match to the row in `to` with the largest area overlap.

When the output does not reference every row of `to`, an attribute
`"matching_max"` is attached and records `nrow(to)`. Functions such as
[`block2prec()`](https://christophertkenny.com/geomander/reference/block2prec.md)
use that attribute to preserve empty target groups.

## Examples

``` r
library(dplyr)
data(checkerboard)
counties <- sf::st_as_sf(as.data.frame(rbind(
  sf::st_union(checkerboard |> filter(i < 4)),
  sf::st_union(checkerboard |> filter(i >= 4))
)))

geo_match(from = checkerboard, to = counties)
#> Warning: Planarizing skipped. `x` missing CRS.
#> Warning: Planarizing skipped. `x` missing CRS.
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
#> [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
geo_match(from = checkerboard, to = counties, method = 'area')
#> Warning: Planarizing skipped. `x` missing CRS.
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
#> [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
```
