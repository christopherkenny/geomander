# Estimate Plans from a Block Assignment File to Voting Districts

District lines are often provided at the census block level, but
analyses often occur at the voting district level. This provides a
simple way to estimate the block level to the voting district level.

## Usage

``` r
baf_to_vtd(baf, plan_name, GEOID = "GEOID", year = 2020)
```

## Arguments

- baf:

  dataframe representing a block assignment file.

- plan_name:

  Name of the column in `baf` containing district assignments.

- GEOID:

  Name of the column containing block GEOIDs, sometimes called
  `"BLOCKID"`. Defaults to `"GEOID"`.

- year:

  Decennial vintage for the crosswalk, either `2010` or `2020`.

## Value

tibble with one row per voting district and the requested plan column

## Details

If a voting district is split between blocks, this currently uses the
most common district.

When a voting district contains blocks assigned to more than one
district, the output uses the modal district assignment.

## Examples

``` r
# Not guaranteed to reach download from redistrict2020.org
if (FALSE) { # \dontrun{
# download and read baf ----
url <- paste0('https://github.com/PlanScore/Redistrict2020/', 
              'raw/main/files/DE-2021-01/DE_SLDU_bef.zip')
tf <- tempfile('.zip')
utils::download.file(url, tf)
utils::unzip(tf, exdir = dirname(tf))
baf <- readr::read_csv(
  file = paste0(dirname(tf), '/DE_SLDU_bef.csv'),
  col_types = 'ci'
)
names(baf) <- c('GEOID', 'ssd_20')

# convert to vtd level ----
baf_to_vtd(baf = baf, plan_name = 'ssd_20', 'GEOID')
} # }
```
