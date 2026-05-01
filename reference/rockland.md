# rockland

This data contains the blocks for Rockland County NY, with geographies
simplified to allow for better examples.

## Usage

``` r
data("rockland")
```

## Format

An sf dataframe with 4764 observations

## Details

It can be recreated with: rockland \<- create_block_table('NY',
'Rockland') rockland \<- rmapshaper::ms_simplify(rockland, keep_shapes =
TRUE)

## Examples

``` r
data('rockland')
```
