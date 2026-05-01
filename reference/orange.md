# orange

This data contains the blocks for Orange County NY, with geographies
simplified to allow for better examples.

## Usage

``` r
data("orange")
```

## Format

An sf dataframe with 10034 observations

## Details

It can be recreated with: orange \<- create_block_table('NY', 'Orange')
orange \<- rmapshaper::ms_simplify(orange, keep_shapes = TRUE)

## Examples

``` r
data('orange')
```
