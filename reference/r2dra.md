# Convert an R Plan to DRA Block Assignment Format

Project a plan defined on precincts down to 2020 census blocks and
return the two-column format expected by DRA exports.

## Usage

``` r
r2dra(precincts, plan, state, path, epsg = 3857)
```

## Arguments

- precincts:

  `sf` object of precinct geometries.

- plan:

  Either a vector of district assignments aligned with `precincts`, or
  the name of a column in `precincts` containing those assignments.

- state:

  State postal abbreviation used to fetch 2020 blocks.

- path:

  Optional output path. If supplied, the result is written as CSV and
  also returned.

- epsg:

  numeric EPSG code to planarize to. Default is 3857.

## Value

tibble with columns `GEOID20` and `District`, suitable for writing to a
DRA-style CSV.

## Examples

``` r
if (FALSE) { # \dontrun{
# Needs Census Bureau API
cd <- tinytiger::tt_congressional_districts() |> filter(STATEFP == '49')
cnty <- tinytiger::tt_counties(state = 49)
matchedcty <- geo_match(from = cnty, to = cd)
# use counties as precincts and let the plan be their center match:
r2dra(cnty, matchedcty, 'UT', 'r2dra_ex.csv')
} # }
```
