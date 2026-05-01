# Check Polygon Contiguity

Cast `shp` to component polygons, build the adjacency, and check the
contiguity. Avoids issues where a precinct is actually a multipolygon

## Usage

``` r
check_polygon_contiguity(shp, group, epsg = 3857)
```

## Arguments

- shp:

  An `sf` dataframe.

- group:

  Unquoted column name in `shp` giving the grouping variable. Use a row
  identifier when checking whether individual rows are multipart.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

tibble in the same format as
[`check_contiguity()`](https://christophertkenny.com/geomander/reference/check_contiguity.md).

## Examples

``` r
data(checkerboard)
check_polygon_contiguity(checkerboard, i)
#> Warning: Planarizing skipped. `x` missing CRS.
#> # A tibble: 64 × 3
#>    group group_number component
#>    <int>        <int>     <int>
#>  1     0            1         1
#>  2     0            1         1
#>  3     0            1         1
#>  4     0            1         1
#>  5     0            1         1
#>  6     0            1         1
#>  7     0            1         1
#>  8     0            1         1
#>  9     1            2         1
#> 10     1            2         1
#> # ℹ 54 more rows
```
