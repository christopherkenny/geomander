# Get an ALARM Dataset

Download a state dataset from the Algorithm-Assisted Redistricting
Methodology Project. Depending on state and year, the data are returned
at either the voting-district level or the census-block level. The
current supported data is the 2020 and 2010 retabulations of the VEST
data, which can be downloaded with
[`get_vest()`](https://christophertkenny.com/geomander/reference/get_vest.md).

## Usage

``` r
get_alarm(state, year = 2020, geometry = TRUE, epsg = 3857)
```

## Arguments

- state:

  Two-letter state abbreviation.

- year:

  Year to retrieve, either `2020` or `2010`.

- geometry:

  Logical. If `TRUE`, join the corresponding geometry.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

tibble or `sf` object with ALARM election data and, optionally,
geometry.

## Details

See the full available data at
<https://github.com/alarm-redist/census-2020>.

## Examples

``` r
ak <- get_alarm('AK', geometry = FALSE)
#> Data sourced from the ALARM Project
#> <https://github.com/alarm-redist/census-2020>.
#> This message is displayed once per session.
```
