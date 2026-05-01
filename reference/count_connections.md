# Count How Often Pairs of Units Share a District

Summarize a district-membership matrix into pairwise co-assignment
counts.

## Usage

``` r
count_connections(dm, normalize = FALSE)
```

## Arguments

- dm:

  District membership matrix, typically with one row per unit and one
  column per plan or draw.

- normalize:

  Logical. If `TRUE`, divide counts by the number of columns in `dm`.

## Value

tibble in long form with columns `x`, `y`, and `fill`, where `fill`
stores the count or proportion of shared assignments.

## Examples

``` r
set.seed(1)
dm <- matrix(sample(1:2, size = 100, TRUE), 10)
count_connections(dm)
#> # A tibble: 100 × 3
#>        x     y  fill
#>    <int> <int> <int>
#>  1     1     1    10
#>  2     1     2     7
#>  3     1     3     4
#>  4     1     4     6
#>  5     1     5     4
#>  6     1     6     6
#>  7     1     7     6
#>  8     1     8     6
#>  9     1     9     3
#> 10     1    10     5
#> # ℹ 90 more rows
```
