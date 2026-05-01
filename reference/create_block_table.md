# Create a Census Block-Level Table

Download block-level decennial census data with the standard
redistricting variables used throughout the package.

## Usage

``` r
create_block_table(
  state,
  county = NULL,
  geometry = TRUE,
  year = 2020,
  mem = FALSE,
  epsg = 3857
)
```

## Arguments

- state:

  Two-letter state postal code.

- county:

  Optional county name or code. If omitted, returns blocks for the
  entire state.

- geometry:

  Logical. If `TRUE`, include geometry.

- year:

  Census year. Intended for decennial years, especially `2010` and
  `2020`.

- mem:

  Logical. If `TRUE`, use the memoized `censable` backend.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

dataframe or `sf` object with one row per block. Includes population and
voting-age-population summaries by race and ethnicity.

## Examples

``` r
if (FALSE) { # \dontrun{
# uses the Census API
create_block_table(state = 'NY', county = 'Rockland', geometry = FALSE)
} # }
```
