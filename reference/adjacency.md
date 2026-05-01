# Build an Adjacency List

This mimics redist's redist.adjacency using geos to create the patterns,
rather than sf. This is faster than that version, but forces
projections.

## Usage

``` r
adjacency(shp, epsg = 3857)
```

## Arguments

- shp:

  `sf` dataframe.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

Zero-indexed adjacency list with `nrow(shp)` entries.

## Examples

``` r
data(precincts)
adj <- adjacency(precincts)
```
