# Suggest Edges to Sew a Seam

Build a set of candidate cross-seam edges by constructing Voronoi cells
from representative points and identifying neighboring cells across the
seam.

## Usage

``` r
seam_sew(shp, admin, seam, epsg = 3857)
```

## Arguments

- shp:

  `sf` object containing the administrative identifier.

- admin:

  Name of the administrative-unit column in `shp`.

- seam:

  Length-2 vector of administrative-unit values defining the seam.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

tibble with columns `v1` and `v2`, suitable for use with
[`add_edge()`](https://christophertkenny.com/geomander/reference/add_edge.md)

## Examples

``` r
data('rockland')
data('orange')
data('nrcsd')

o_and_r <- rbind(orange, rockland)
o_and_r <- o_and_r |>
  geo_filter(nrcsd) |>
  geo_trim(nrcsd)
adj <- adjacency(o_and_r)

adds <- seam_sew(o_and_r, 'county', c('071', '087'))
adj <- adj |> add_edge(adds$v1, adds$v2)
```
