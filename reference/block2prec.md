# Aggregate a Block Table by Matches

Aggregate block-level attributes up to a larger geography, usually
precincts, using a vector of group assignments such as the output of
[`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md).

## Usage

``` r
block2prec(block_table, matches, geometry = FALSE)
```

## Arguments

- block_table:

  Block table, usually from
  [`create_block_table()`](https://christophertkenny.com/geomander/reference/create_block_table.md).

- matches:

  Integer grouping variable, typically from
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md).

- geometry:

  Logical. If `TRUE`, union geometry within each matched group.

## Value

dataframe with one row per matched group. When empty target groups are
preserved, the number of rows may be larger than
`length(unique(matches))`.

## Details

If `matches` carries a `"matching_max"` attribute, the output preserves
all target groups up to that size, even when some groups receive no
matched rows. Missing nonnegative numeric summaries are filled with `0`.

## Examples

``` r
set.seed(1)
data(rockland)
rockland$id <- sample(c(1:2, 4), nrow(rockland), TRUE)
block2prec(rockland, rockland$id)
#> # A tibble: 4 × 24
#>   matches_id waterpct    pop pop_white pop_black pop_hisp pop_aian pop_asian
#>        <dbl>    <dbl>  <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1          1     40.1 108145     71422     12109    16272      170      6486
#> 2          2     48.3 103132     68520     10470    16357      140      5939
#> 3          3      0        0         0         0        0        0         0
#> 4          4     37.7 100410     63728     12044    16154      177      6674
#> # ℹ 16 more variables: pop_nhpi <dbl>, pop_other <dbl>, pop_two <dbl>,
#> #   vap <dbl>, vap_white <dbl>, vap_black <dbl>, vap_hisp <dbl>,
#> #   vap_aian <dbl>, vap_asian <dbl>, vap_nhpi <dbl>, vap_other <dbl>,
#> #   vap_two <dbl>, place <dbl>, id <dbl>, state <chr>, county <chr>
```
