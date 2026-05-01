# Suggest Connections for Disconnected Groups

Suggest nearest cross-component links that could reconnect disconnected
groups.

## Usage

``` r
suggest_component_connection(shp, adj, group, epsg = 3857)
```

## Arguments

- shp:

  An `sf` dataframe.

- adj:

  Adjacency list.

- group:

  Optional vector of group identifiers. If omitted, all rows are treated
  as belonging to one group.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

tibble with columns `x` and `y`, giving candidate row pairs to connect
with
[`add_edge()`](https://christophertkenny.com/geomander/reference/add_edge.md).

## Examples

``` r
library(dplyr)
data(checkerboard)
checkerboard <- checkerboard |> filter(i != 1, j != 1)
adj <- adjacency(checkerboard)
#> Warning: Planarizing skipped. `x` missing CRS.
suggest_component_connection(checkerboard, adj)
#> Warning: Planarizing skipped. `x` missing CRS.
#> Warning: Planarizing skipped. `x` missing CRS.
#> # A tibble: 3 × 2
#>       x     y
#>   <int> <int>
#> 1     2     9
#> 2     1     2
#> 3     1     8
```
