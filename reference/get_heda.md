# Get a Harvard Election Data Archive ("HEDA") Dataset

Download a state file from the Harvard Election Data Archive and return
it as an `sf` object. Some states require special handling because the
source files differ in format or level of geography.

## Usage

``` r
get_heda(state, path = tempdir(), epsg = 3857, ...)
```

## Arguments

- state:

  Two-letter state abbreviation.

- path:

  Directory used for extracted files. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

- ...:

  Additional arguments passed to
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html).

## Value

`sf` tibble containing the requested HEDA dataset

## Details

Most states are distributed as zipped shapefiles. California is stored
differently and is aggregated here to 2010 tracts. Some states are
shipped as loose shapefile components rather than zip archives. If the
source file has no CRS metadata, the function assumes EPSG 4140 before
optionally reprojecting.

The function currently returns the raw source column names because
internal name cleaning is disabled in the implementation.

## Examples

``` r
if (FALSE) { # Sys.getenv("DATAVERSE_KEY") != ""
shp <- get_heda('ND')
}
```
