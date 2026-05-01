# Clean VEST Column Names

Rename VEST election-result columns into a more uniform
`office_year_party_candidate` style.

## Usage

``` r
clean_vest(data)
```

## Arguments

- data:

  `sf` tibble from
  [`get_vest()`](https://christophertkenny.com/geomander/reference/get_vest.md).

## Value

input object with renamed columns

## Examples

``` r
data(va18sub)
va <- clean_vest(va18sub)
```
