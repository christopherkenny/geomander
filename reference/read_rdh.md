# Read a Redistricting Data Hub ("RDH") Dataset

Read a state-year RDH precinct file.

## Usage

``` r
read_rdh(file, path = tempdir(), clean_names = TRUE, epsg = 3857, ...)
```

## Arguments

- file:

  Path to an RDH file, either zipped or a file readable by
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html),
  such as a `.shp`.

- path:

  Directory used for extracted files. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- clean_names:

  Logical. If `TRUE`, rename election columns into a more consistent
  scheme.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

- ...:

  Additional arguments passed to
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html).

## Value

`sf` tibble containing the RDH dataset

## Examples

``` r
if (FALSE) { # \dontrun{
shp <- read_rdh('in_2024_gen_prec.zip')
} # }
```
