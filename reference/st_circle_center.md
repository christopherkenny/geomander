# Get the Center of the Maximum Inscribed Circle

Compute the centroid of the largest inscribed circle for each feature.

## Usage

``` r
st_circle_center(shp, tolerance = 0.01, epsg = 3857)
```

## Arguments

- shp:

  An `sf` dataframe.

- tolerance:

  positive numeric tolerance to simplify by. Default is 0.01.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

`sf` dataframe with the same rows and attributes as `shp`, but point
geometries.

## Examples

``` r
data(towns)
st_circle_center(towns)
#> Simple feature collection with 7 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8248447 ymin: 5041227 xmax: -8233259 ymax: 5049609
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>          NAME         VILLAGE        TOWN               MUNI ID
#> 1  Haverstraw      Haverstraw  Haverstraw Haverstraw Village  1
#> 2  Haverstraw          Pomona  Haverstraw Pomona, Haverstraw  2
#> 3  Haverstraw            <NA>  Haverstraw         Haverstraw  3
#> 4  Haverstraw West Haverstraw  Haverstraw    West Haverstraw  4
#> 5  Haverstraw            <NA>  Haverstraw         Haverstraw  5
#> 6 Stony Point            <NA> Stony Point        Stony Point  6
#> 7  Haverstraw            <NA>  Haverstraw         Haverstraw  7
#>                   geometry
#> 1 POINT (-8234582 5041227)
#> 2 POINT (-8242964 5041868)
#> 3 POINT (-8235722 5044015)
#> 4 POINT (-8236606 5043118)
#> 5 POINT (-8248447 5043738)
#> 6 POINT (-8240068 5049609)
#> 7 POINT (-8233259 5043748)
```
