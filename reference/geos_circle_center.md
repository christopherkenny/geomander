# Get the centroid of the maximum inscribed circle

Returns the centroid of the largest inscribed circle for each shape

## Usage

``` r
geos_circle_center(shp, tolerance = 0.01, epsg = 3857)
```

## Arguments

- shp:

  An `sf` dataframe.

- tolerance:

  positive numeric tolerance to simplify by. Default is 0.01.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

geos geometry vector of points.

## Examples

``` r
data(towns)
geos_circle_center(towns)
#> <geos_geometry[7] with CRS=WGS 84 / Pseudo-Mercator>
#> [1] <POINT (-8234581.81061 5041227.20796)>
#> [2] <POINT (-8242964.41612 5041867.6098)> 
#> [3] <POINT (-8235721.66668 5044014.65337)>
#> [4] <POINT (-8236606.26276 5043118.13727)>
#> [5] <POINT (-8248446.64048 5043738.24472)>
#> [6] <POINT (-8240067.5504 5049608.51069)> 
#> [7] <POINT (-8233258.86813 5043748.1357)> 
```
