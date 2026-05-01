# Get Historical United States Congressional District Shapefiles

Data sourced from the United States Congressional District Shapefiles,
primarily hosted at <https://cdmaps.polisci.ucla.edu/>. Files are
fetched through the GitHub repository at
<https://github.com/JeffreyBLewis/congressional-district-boundaries>.

## Usage

``` r
get_lewis(state, congress, path_only = FALSE)
```

## Arguments

- state:

  State name, FIPS, or two-letter abbreviation understood by `censable`.

- congress:

  Congress number to retrieve.

- path_only:

  Logical. If `TRUE`, return only the selected download URL.

## Value

`sf` tibble of congressional district boundaries, or a character URL
when `path_only = TRUE`.

## References

Jeffrey B. Lewis, Brandon DeVine, Lincoln Pitcher, and Kenneth C.
Martis. (2013) Digital Boundary Definitions of United States
Congressional Districts, 1789-2012. \[Data file and code book\].
Retrieved from <https://cdmaps.polisci.ucla.edu> on \[date of
download\].

## Examples

``` r
path <- get_lewis(state = 'NM', congress = 111, path_only = TRUE)
#> Data sourced from the United States Congressional District Shapefiles
#> <https://cdmaps.polisci.ucla.edu/>.
#> This message is displayed once per session.
if (attr(curlGetHeaders(path), 'status') == 200) {
  get_lewis(state = 'NM', congress = 111)
}
#> Simple feature collection with 3 features and 16 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -109.0502 ymin: 31.3323 xmax: -103.002 ymax: 37.00023
#> Geodetic CRS:  NAD83
#> # A tibble: 3 × 17
#>   statename district startcong endcong id    districtsi county page  law   note 
#>   <chr>        <dbl>     <dbl>   <dbl> <chr> <chr>      <chr>  <chr> <chr> <chr>
#> 1 New Mexi…        1       108     112 0351… ""         ""     ""    ""    ""   
#> 2 New Mexi…        2       108     112 0351… ""         ""     ""    ""    ""   
#> 3 New Mexi…        3       108     112 0351… ""         ""     ""    ""    ""   
#> # ℹ 7 more variables: bestdec <chr>, finalnote <chr>, rnote <chr>,
#> #   lastchange <dttm>, fromcounty <chr>, statefp <chr>, geometry <POLYGON [°]>
```
