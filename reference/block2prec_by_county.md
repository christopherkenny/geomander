# Aggregate a Block Table by Matches Within County

Variant of
[`block2prec()`](https://christophertkenny.com/geomander/reference/block2prec.md)
that matches blocks to precincts separately within each county. This
helps avoid cross-county mismatches when county boundaries and census
blocks do not line up cleanly.

## Usage

``` r
block2prec_by_county(block_table, precinct, precinct_county_fips, epsg = 3857)
```

## Arguments

- block_table:

  Block table, usually from
  [`create_block_table()`](https://christophertkenny.com/geomander/reference/create_block_table.md).

- precinct:

  `sf` object of target precinct shapes.

- precinct_county_fips:

  Name of the county identifier column in `precinct`.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

dataframe with one row per row of `precinct`, ordered to align with the
input precinct object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Need Census API
data(towns)
towns$fips <- '087'
block <- create_block_table('NY', 'Rockland')
block2prec_by_county(block, towns, 'fips')
} # }
```
