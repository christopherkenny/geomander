# Trim Away Small Pieces

Keep only rows of `from` whose intersection with `to` covers more than a
given share of the row's area.

## Usage

``` r
geo_trim(from, to, thresh = 0.01, bool = FALSE, epsg = 3857)
```

## Arguments

- from:

  `sf` object to subset.

- to:

  `sf` object used as the target geography.

- thresh:

  Minimum retained overlap as a proportion of each row's area.

- bool:

  Logical. If `TRUE`, return only the logical keep vector.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

subset of `from`, or a logical vector when `bool = TRUE`

## Examples

``` r
if (FALSE) { # \dontrun{
# Needs Census Bureau API
data(towns)
block <- create_block_table('NY', 'Rockland')
geo_trim(block, towns, thresh = 0.05)
} # }

data(towns)
data(rockland)
sub <- geo_filter(rockland, towns)
rem <- geo_trim(sub, towns, thresh = 0.05)
```
