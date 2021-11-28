
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geomander <a href='https://www.christophertkenny.com/geomander/'><img src='man/figures/logo.png' align="right" height="137" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/christopherkenny/geomander/workflows/R-CMD-check/badge.svg)](https://github.com/christopherkenny/geomander/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/geomander)](https://CRAN.R-project.org/package=geomander)
![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/geomander)
<!-- badges: end -->

Focuses on creating data sets and other tools that help make
understanding gerrymandering faster and easier. Designed for easy
preparation to run simulation analysis with the R package redist, but is
aimed at the geographic aspects of redistricting, not partitioning
methods. Most of these tools are gathered from seminar papers and do not
correspond to a single publication.

## Installation

You can install the released version of geomander from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("geomander")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("christopherkenny/geomander")
```

## Examples

A very common task is aggregating block data to precincts.

``` r
library(geomander)
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
 
# load precincts
data('va18sub')

# create block data
block <- create_block_table(state = 'VA', county = '087')  
#> Getting data from the 2020 decennial Census
#> Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.
#> Using the PL 94-171 Redistricting Data summary file
#> Note: 2020 decennial Census data use differential privacy, a technique that
#> introduces errors into data to preserve respondent confidentiality.
#> i Small counts should be interpreted with caution.
#> i See https://www.census.gov/library/fact-sheets/2021/protecting-the-confidentiality-of-the-2020-census-redistricting-data.html for additional guidance.
#> This message is displayed once per session.

# match the geographies
matches <- geo_match(from = block, to = va18sub)

# Aggregate
prec <- block2prec(block_table = block, matches = matches)
```

Other important tasks include breaking data into pieces by blocks
underlying them.

``` r
library(geomander)
library(tidyverse)
 
# load precincts
data("va18sub")

# subset to target area
va18sub <- va18sub %>% filter(COUNTYFP == '087')
```

Then we can get common block data:

``` r
block <- create_block_table(state = 'VA', county = '087')  
```

And estimate down to blocks

``` r
disagg <- geo_estimate_down(from = va18sub, to = block, wts = block$vap, value = va18sub$G18USSRSTE)
```

For more information, see the documentation and vignettes, available at
<https://www.christophertkenny.com/geomander/>
