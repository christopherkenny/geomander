# Get a Voting and Election Science Team ("VEST") Dataset

Download a state-year VEST precinct file from Harvard Dataverse.

## Usage

``` r
get_vest(state, year, path = tempdir(), clean_names = TRUE, epsg = 3857, ...)
```

## Arguments

- state:

  Two-letter state abbreviation.

- year:

  Election year, currently one of `2016` through `2021`.

- path:

  Directory used for extracted files. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- clean_names:

  Logical. If `TRUE`, rename election columns into a more consistent
  scheme.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

- ...:

  Additional arguments passed to
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html).

## Value

`sf` tibble containing the requested VEST dataset

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires Dataverse API
shp <- get_vest('CO', 2020)
} # }
```
