# Get the kind of center of each shape

Returns points within the shape, near the center. Uses the centroid if
that's in the shape, or point on surface if not.

## Usage

``` r
geos_centerish(shp, epsg = 3857)
```

## Arguments

- shp:

  An `sf` dataframe.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

geos geometry vector of points.

## Examples

``` r
data(towns)
geos_centerish(towns)
#> <geos_geometry[7] with CRS=WGS 84 / Pseudo-Mercator>
#> [1] <POINT (-8234038.72204 5040736.91813)>
#> [2] <POINT (-8243080.69709 5041725.31103)>
#> [3] <POINT (-8235790.95729 5044025.01539)>
#> [4] <POINT (-8236353.90968 5042820.13014)>
#> [5] <POINT (-8246025.59424 5043154.17798)>
#> [6] <POINT (-8239546.93907 5050559.6919)> 
#> [7] <POINT (-8234888.29266 5049278.86145)>
```
