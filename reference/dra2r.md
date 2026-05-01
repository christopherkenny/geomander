# Convert DRA Export Data to an R Spatial Object

Read a DRA block assignment export and attach it to 2020 census blocks,
optionally collapsing the assignments to a precinct layer.

## Usage

``` r
dra2r(dra, state, precincts, epsg = 3857)
```

## Arguments

- dra:

  Path to a DRA CSV export, or a dataframe containing columns `GEOID20`
  and `District`.

- state:

  State postal abbreviation used to fetch 2020 blocks.

- precincts:

  Optional `sf` object of precinct shapes. If supplied, the function
  assigns each precinct the most common block-level district among the
  blocks matched to it.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

`sf` object at the block level when `precincts` is omitted, or at the
precinct level when `precincts` is supplied.

## Examples

``` r
if (FALSE) { # \dontrun{
# Needs Census Bureau API
# dra_utah_test is available at https://bit.ly/3c6UDKk
blocklevel <- dra2r('dra_utah_test.csv', state = 'UT')
} # }
```
