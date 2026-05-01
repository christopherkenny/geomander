# va_blocks

This data contains the blocks Henrico County, VA with geographies
simplified to allow for better examples.

## Usage

``` r
data("va_blocks")
```

## Format

An sf dataframe with 6354 observations

## Details

blocks87 \<- create_block_table(state = 'VA', county = '087') va_blocks
\<- rmapshaper::ms_simplify(va_blocks, keep_shapes = TRUE)

## Examples

``` r
data('va_blocks')
```
