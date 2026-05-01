# Filter to Intersecting Pieces

Keep only the rows of `from` whose geometry intersects any geometry in
`to`.

## Usage

``` r
geo_filter(from, to, bool = FALSE, epsg = 3857)
```

## Arguments

- from:

  `sf` object to subset.

- to:

  `sf` object used as the target geography.

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
geo_filter(block, towns)
} # }

data(towns)
data(rockland)
sub <- geo_filter(rockland, towns)
```
