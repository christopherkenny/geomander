# va_vtd

This data contains the blocks for Henrico County, VA with geographies
simplified to allow for better examples.

## Usage

``` r
data("va_blocks")
```

## Format

An sf dataframe with 93 observations

## Details

va_vtd \<- tinytiger::tt_voting_districts(state = 'VA', county = '087',
year = 2010) va_vtd \<- rmapshaper::ms_simplify(va_vtd, keep_shapes =
TRUE)

## Examples

``` r
data('va_blocks')
```
