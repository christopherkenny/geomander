# Get the kind of center of each shape

Returns points within the shape, near the center. Uses the centroid if
that's in the shape, or point on surface if not.

## Usage

``` r
st_centerish(shp, epsg = 3857)
```

## Arguments

- shp:

  An `sf` dataframe.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

`sf` dataframe with the same rows and attributes as `shp`, but point
geometries.

## Examples

``` r
data(towns)
st_centerish(towns)
#> Simple feature collection with 7 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8246026 ymin: 5040737 xmax: -8234039 ymax: 5050560
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
#> 1 POINT (-8234039 5040737)
#> 2 POINT (-8243081 5041725)
#> 3 POINT (-8235791 5044025)
#> 4 POINT (-8236354 5042820)
#> 5 POINT (-8246026 5043154)
#> 6 POINT (-8239547 5050560)
#> 7 POINT (-8234888 5049279)
```
