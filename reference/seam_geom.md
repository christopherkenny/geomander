# Filter Shapes to Units Along a Seam

Keep only rows in `shp` that lie on the border between two
administrative units and have at least one adjacency connection across
that border.

## Usage

``` r
seam_geom(adj, shp, admin, seam, epsg = 3857)
```

## Arguments

- adj:

  Zero-indexed adjacency graph.

- shp:

  `sf` object containing the administrative identifier.

- admin:

  Name of the administrative-unit column in `shp`.

- seam:

  Length-2 vector of administrative-unit values defining the seam.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

subset of `shp` containing only rows on the selected seam

## Examples

``` r
data('rockland')
data('orange')
data('nrcsd')

o_and_r <- rbind(orange, rockland)
o_and_r <- o_and_r |>
  geo_filter(nrcsd) |>
  geo_trim(nrcsd)
adj <- adjacency(o_and_r)

seam_geom(adj, shp = o_and_r, admin = 'county', seam = c('071', '087'))
#> Simple feature collection with 24 features and 25 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -8247177 ymin: 5049265 xmax: -8235418 ymax: 5060742
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> First 10 features:
#>      state county  tract block           GEOID waterpct pop pop_white pop_black
#> 486     36    071 013800  3031 360710138003031        0   0         0         0
#> 660     36    071 013800  3066 360710138003066        0   0         0         0
#> 668     36    071 013800  3045 360710138003045        0   0         0         0
#> 937     36    071 013900  3113 360710139003113        0   0         0         0
#> 2478    36    071 013800  3032 360710138003032        0   0         0         0
#> 4278    36    071 013900  3108 360710139003108        0   0         0         0
#> 4448    36    071 014900  1091 360710149001091        0   0         0         0
#> 5332    36    071 013800  3036 360710138003036        1   0         0         0
#> 5334    36    071 013800  3044 360710138003044        1   0         0         0
#> 5533    36    071 013800  3043 360710138003043        1   0         0         0
#>      pop_hisp pop_aian pop_asian pop_nhpi pop_other pop_two vap vap_white
#> 486         0        0         0        0         0       0   0         0
#> 660         0        0         0        0         0       0   0         0
#> 668         0        0         0        0         0       0   0         0
#> 937         0        0         0        0         0       0   0         0
#> 2478        0        0         0        0         0       0   0         0
#> 4278        0        0         0        0         0       0   0         0
#> 4448        0        0         0        0         0       0   0         0
#> 5332        0        0         0        0         0       0   0         0
#> 5334        0        0         0        0         0       0   0         0
#> 5533        0        0         0        0         0       0   0         0
#>      vap_black vap_hisp vap_aian vap_asian vap_nhpi vap_other vap_two place
#> 486          0        0        0         0        0         0       0 99999
#> 660          0        0        0         0        0         0       0 99999
#> 668          0        0        0         0        0         0       0 99999
#> 937          0        0        0         0        0         0       0 82750
#> 2478         0        0        0         0        0         0       0 99999
#> 4278         0        0        0         0        0         0       0 82750
#> 4448         0        0        0         0        0         0       0 99999
#> 5332         0        0        0         0        0         0       0 99999
#> 5334         0        0        0         0        0         0       0 99999
#> 5533         0        0        0         0        0         0       0 99999
#>                            geometry
#> 486  MULTIPOLYGON (((-8237395 50...
#> 660  MULTIPOLYGON (((-8238303 50...
#> 668  MULTIPOLYGON (((-8238234 50...
#> 937  MULTIPOLYGON (((-8241105 50...
#> 2478 MULTIPOLYGON (((-8237787 50...
#> 4278 MULTIPOLYGON (((-8241634 50...
#> 4448 MULTIPOLYGON (((-8247157 50...
#> 5332 MULTIPOLYGON (((-8236666 50...
#> 5334 MULTIPOLYGON (((-8236121 50...
#> 5533 MULTIPOLYGON (((-8236047 50...
```
