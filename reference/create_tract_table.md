# Create an ACS Tract-Level Table

Download tract-level ACS data with the standard redistricting variables
used throughout the package.

## Usage

``` r
create_tract_table(
  state,
  county,
  geometry = TRUE,
  year = 2019,
  mem = FALSE,
  epsg = 3857
)
```

## Arguments

- state:

  Two-letter state postal code.

- county:

  Optional county name or code. If omitted, returns tracts for the
  entire state.

- geometry:

  Logical. If `TRUE`, include geometry.

- year:

  ACS year, currently intended for `2009` through `2019`.

- mem:

  Logical. If `TRUE`, use the memoized `censable` backend.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

dataframe or `sf` object with one row per tract. Includes population,
voting-age population, and citizen voting-age population summaries by
race and ethnicity.

## Examples

``` r
if (FALSE) { # \dontrun{
# Relies on Census Bureau API
tract <- create_tract_table('NY', 'Rockland', year = 2018)
} # }
```
