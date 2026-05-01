# Suggest Neighbors for Lonely Precincts

For precincts which have no adjacent precincts, this suggests the
nearest precinct as a friend to add. This is useful for when a small
number of precincts are disconnected from the remainder of the
geography, such as an island.

## Usage

``` r
suggest_neighbors(shp, adj, idx, neighbors = 1)
```

## Arguments

- shp:

  `sf` object used to compute representative points.

- adj:

  Adjacency list.

- idx:

  Optional integer vector of row indices to repair. If omitted, the
  function uses rows with no neighbors.

- neighbors:

  Number of candidate neighbors to return for each row in `idx`.

## Value

tibble with columns `x` and `y`, giving pairs of row indices that could
be connected with
[`add_edge()`](https://christophertkenny.com/geomander/reference/add_edge.md).

## Examples

``` r
library(dplyr)
data(va18sub)
va18sub <- va18sub |> filter(!VTDST %in% c('000516', '000510', '000505', '000518'))
adj <- adjacency(va18sub)
suggests <- suggest_neighbors(va18sub, adj)
adj <- adj |> add_edge(v1 = suggests$x, v2 = suggests$y)
```
