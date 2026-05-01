# Sort Features by Northwest-to-Southeast Order

Reorder rows by the centroid distance from the northwest corner of the
overall bounding box.

## Usage

``` r
geo_sort(shp, epsg = 3857)
```

## Arguments

- shp:

  `sf` dataframe.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

reordered `sf` dataframe

## Examples

``` r
data(checkerboard)
geo_sort(checkerboard)
#> Warning: Planarizing skipped. `x` missing CRS.
#> Simple feature collection with 64 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 8 ymax: 8
#> CRS:           NA
#> First 10 features:
#>    i j id                       geometry
#> 1  0 7  8 POLYGON ((0 7, 1 7, 1 8, 0 ...
#> 2  0 6  7 POLYGON ((0 6, 1 6, 1 7, 0 ...
#> 3  1 7 16 POLYGON ((1 7, 2 7, 2 8, 1 ...
#> 4  1 6 15 POLYGON ((1 6, 2 6, 2 7, 1 ...
#> 5  0 5  6 POLYGON ((0 5, 1 5, 1 6, 0 ...
#> 6  2 7 24 POLYGON ((2 7, 3 7, 3 8, 2 ...
#> 7  1 5 14 POLYGON ((1 5, 2 5, 2 6, 1 ...
#> 8  2 6 23 POLYGON ((2 6, 3 6, 3 7, 2 ...
#> 9  0 4  5 POLYGON ((0 4, 1 4, 1 5, 0 ...
#> 10 2 5 22 POLYGON ((2 5, 3 5, 3 6, 2 ...
```
