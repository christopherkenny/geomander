# Compare Adjacency Lists

Compare two adjacency lists and report edges that differ between them.

## Usage

``` r
compare_adjacencies(adj1, adj2, shp, zero = TRUE)
```

## Arguments

- adj1:

  First adjacency list.

- adj2:

  Second adjacency list.

- shp:

  Optional `sf` object used to compute DE-9IM relations for differing
  pairs.

- zero:

  Logical. `TRUE` when the adjacency lists are zero-indexed.

## Value

tibble of differing edge pairs. When `shp` is supplied, includes DE-9IM
relation and simple intersection class diagnostics.

## Examples

``` r
data(towns)
rook <- adjacency(towns)
sf_rook <- lapply(sf::st_relate(towns, pattern = 'F***1****'), function(x) {
  x - 1L
})
#> although coordinates are longitude/latitude, st_relate_pattern assumes that
#> they are planar
compare_adjacencies(rook, sf_rook, zero = FALSE)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: x <int>, y <dbl>, from <dbl>, relation <chr>, class <chr>
```
