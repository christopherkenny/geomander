# Estimate Regions Separated by Geographic Features

Divide an adjacency graph into regions by removing edges whose
centerlines intersect a set of line features, such as roads or rivers.

## Usage

``` r
regionalize(shp, lines, adj = adjacency(shp), epsg = 3857)
```

## Arguments

- shp:

  `sf` object to regionalize.

- lines:

  `sf` line object representing separators.

- adj:

  Optional zero-indexed adjacency graph. Defaults to `adjacency(shp)`.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

integer vector of region labels with `nrow(shp)` entries

## Examples

``` r
data(towns)
# make some weird roadlike feature passing through the towns
lines <- sf::st_sfc(sf::st_linestring(sf::st_coordinates(sf::st_centroid(towns))),
  crs = sf::st_crs(towns)
)
#> Warning: st_centroid assumes attributes are constant over geometries
regionalize(towns, lines)
#> [1] 1 2 1 1 2 2 2
```
