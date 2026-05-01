# Add Edges to an Adjacency List

Add one or more undirected edges to an adjacency list in place and
return the modified list.

## Usage

``` r
add_edge(adj, v1, v2, ids = NULL, zero = TRUE)
```

## Arguments

- adj:

  List of neighbors, one entry per vertex.

- v1:

  vector of vertex identifiers for the first vertex. Can be an integer
  index or a value to look up in `ids`, if that argument is provided. If
  more than one identifier is present, connects each to corresponding
  entry in v2.

- v2:

  vector of vertex identifiers for the second vertex. Can be an integer
  index or a value to look up in `ids`, if that argument is provided. If
  more than one identifier is present, connects each to corresponding
  entry in v2.

- ids:

  Optional identifier vector used to look up row indices. If provided,
  each entry in `v1` and `v2` must match exactly one entry.

- zero:

  Logical. `TRUE` when the adjacency list stores zero-based neighbor
  indices and `FALSE` when it stores one-based indices.

## Value

Adjacency list with the requested edges added symmetrically.

## Examples

``` r
data(towns)
adj <- adjacency(towns)

add_edge(adj, 2, 3)
#> [[1]]
#> [1] 3 4 6
#> 
#> [[2]]
#> [1] 4 2
#> 
#> [[3]]
#> [1] 3 5 1
#> 
#> [[4]]
#> [1] 0 4 2 6 5
#> 
#> [[5]]
#> [1] 0 1 3 5
#> 
#> [[6]]
#> [1] 3 4 2 6
#> 
#> [[7]]
#> [1] 0 3 5
#> 
add_edge(adj, "West Haverstraw", "Stony Point", towns$MUNI)
#> [[1]]
#> [1] 3 4 6
#> 
#> [[2]]
#> [1] 4
#> 
#> [[3]]
#> [1] 3 5
#> 
#> [[4]]
#> [1] 0 4 2 6 5 5
#> 
#> [[5]]
#> [1] 0 1 3 5
#> 
#> [[6]]
#> [1] 3 4 2 6 3
#> 
#> [[7]]
#> [1] 0 3 5
#> 
```
