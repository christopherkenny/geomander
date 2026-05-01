# Get the RPV Near Me Dataset

Download precinct-level racially polarized voting estimates from the RPV
Near Me repository.

## Usage

``` r
get_rpvnearme(state, version = c(1, 2))
```

## Arguments

- state:

  State postal code.

- version:

  Dataset version. Use `1` for the original release or `2` for the
  extended release.

## Value

tibble of precinct-level estimates of party vote by race

## Examples

``` r
get_rpvnearme('DE')
#> Data sourced from the RPV Near Me <https://www.rpvnearme.org/>.
#> This message is displayed once per session.
#> # A tibble: 411 × 58
#>    county      GEOID       vap_white.pre_16_dem_cli vap_white.pre_16_rep_tru
#>    <chr>       <chr>                          <dbl>                    <dbl>
#>  1 Kent County 10001001-28                    0.318                    0.682
#>  2 Kent County 10001001-29                    0.188                    0.812
#>  3 Kent County 10001001-30                    0.178                    0.822
#>  4 Kent County 10001001-31                    0.253                    0.747
#>  5 Kent County 10001001-32                    0.304                    0.696
#>  6 Kent County 10001001-33                    0.278                    0.722
#>  7 Kent County 10001001-34                    0.192                    0.808
#>  8 Kent County 10001002-28                    0.311                    0.689
#>  9 Kent County 10001002-29                    0.330                    0.670
#> 10 Kent County 10001002-30                    0.260                    0.740
#> # ℹ 401 more rows
#> # ℹ 54 more variables: vap_black.pre_16_dem_cli <dbl>,
#> #   vap_black.pre_16_rep_tru <dbl>, vap_hisp.pre_16_dem_cli <dbl>,
#> #   vap_hisp.pre_16_rep_tru <dbl>, vap_oth.pre_16_dem_cli <dbl>,
#> #   vap_oth.pre_16_rep_tru <dbl>, vap_white.gov_16_dem_car <dbl>,
#> #   vap_white.gov_16_rep_bon <dbl>, vap_black.gov_16_dem_car <dbl>,
#> #   vap_black.gov_16_rep_bon <dbl>, vap_hisp.gov_16_dem_car <dbl>, …
```
