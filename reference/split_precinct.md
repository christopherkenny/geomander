# Split a Precinct

States often split a precinct when they create districts but rarely
provide the geography for the split precinct. This allows you to split a
precinct using a lower geography, typically blocks.

## Usage

``` r
split_precinct(lower, precinct, split_by, lower_wt, split_by_id, epsg = 3857)
```

## Arguments

- lower:

  Lower geography that makes up the precinct, often blocks.

- precinct:

  Single-row `sf` object giving the precinct to split.

- split_by:

  Geography that defines the pieces to split the precinct into.

- lower_wt:

  Optional numeric vector of length `nrow(lower)` to aggregate within
  the split pieces, such as population or VAP.

- split_by_id:

  Optional column name in `split_by` to copy onto the output.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

`sf` dataframe with one row per observed split piece

## Details

The function first filters `lower` and `split_by` to the selected
precinct, matches lower-level units to `split_by`, and unions matched
pieces. When `lower_wt` is supplied, a `wt` column is added with summed
weights.

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
data(checkerboard)
low <- checkerboard |> dplyr::slice(1:3, 9:11)
prec <- checkerboard |>
  dplyr::slice(1:3) |>
  dplyr::summarize(geometry = sf::st_union(geometry))
dists <- checkerboard |>
  dplyr::slice(1:3, 9:11) |>
  dplyr::mutate(dist = c(1, 2, 2, 1, 3, 3)) |>
  dplyr::group_by(dist) |>
  dplyr::summarize(geometry = sf::st_union(geometry))

split_precinct(low, prec, dists, split_by_id = 'dist')
#> Warning: Planarizing skipped. `x` missing CRS.
#> Warning: Planarizing skipped. `x` missing CRS.
#> Simple feature collection with 2 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 3
#> CRS:           NA
#> # A tibble: 2 × 3
#>     new                              geometry    id
#> * <int>                             <POLYGON> <dbl>
#> 1     1           ((0 0, 1 0, 1 1, 0 1, 0 0))     1
#> 2     2 ((1 1, 0 1, 0 2, 0 3, 1 3, 1 2, 1 1))     2
```
