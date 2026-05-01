# Get a Dave's Redistricting App Dataset

Download a state election dataset from the DRA public repository.
Depending on state and year, the geometry may be joined at the VTD or
block-group level.

## Usage

``` r
get_dra(state, year = 2020, geometry = TRUE, clean_names = TRUE, epsg = 3857)
```

## Arguments

- state:

  Two-letter state abbreviation.

- year:

  Year to retrieve, either `2020` or `2010`.

- geometry:

  Logical. If `TRUE`, join the corresponding geometry.

- clean_names:

  Logical. If `TRUE`, rename election columns into a more consistent
  scheme.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

tibble or `sf` object with DRA election data and, optionally, geometry.

## Details

See the full available data at <https://github.com/dra2020/vtd_data>.

## Examples

``` r
ak <- get_dra('AK', geometry = FALSE)
#> Data sourced from Dave's Redistricting <https://github.com/dra2020/vtd_data>.
#> This message is displayed once per session.
```
