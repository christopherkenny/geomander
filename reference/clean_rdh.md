# Clean RDH Column Names

Rename RDH election-result columns into a more uniform
`office_year_party_candidate` style.

## Usage

``` r
clean_rdh(data, year = NULL)
```

## Arguments

- data:

  `sf` tibble from
  [`read_rdh()`](https://christophertkenny.com/geomander/reference/read_rdh.md).

- year:

  Election year for district columns without a year, like `GCON01DMRV`.
  If `NULL`, inferred from the other columns.

## Value

input object with renamed columns

## Examples

``` r
if (FALSE) { # \dontrun{
shp <- read_rdh('in_2024_gen_prec.zip', clean_names = FALSE)
shp <- clean_rdh(shp)
} # }
```
