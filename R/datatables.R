#' Create a Census Block-Level Table
#'
#' Download block-level decennial census data with the standard redistricting
#' variables used throughout the package.
#'
#' @param state Two-letter state postal code.
#' @param county Optional county name or code. If omitted, returns blocks for the
#'   entire state.
#' @param geometry Logical. If `TRUE`, include geometry.
#' @param year Census year. Intended for decennial years, especially `2010` and
#'   `2020`.
#' @param mem Logical. If `TRUE`, use the memoized `censable` backend.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return dataframe or `sf` object with one row per block. Includes population
#'   and voting-age-population summaries by race and ethnicity.
#'
#' @export
#' @concept datatable
#' @examples
#' \dontrun{
#' # uses the Census API
#' create_block_table(state = 'NY', county = 'Rockland', geometry = FALSE)
#' }
create_block_table <- function(state, county = NULL, geometry = TRUE, year = 2020,
                               mem = FALSE, epsg = 3857) {
  statepo <- censable::match_abb(state)

  if (length(statepo) == 0) {
    cli::cli_abort('Please provide a two letter postal abbreviation for the {.arg state}.')
  }

  if (year < 2000 || year %% 10 != 0) {
    cli::cli_warn('Only 2010 and 2020 are currently supported. 2000 may or may not work.')
  }

  if (mem) {
    out <- censable::mem_build_dec(
      geography = 'block', state = state, county = county,
      geometry = geometry, year = year, groups = 'all'
    )
  } else {
    out <- censable::build_dec(
      geography = 'block', state = state, county = county,
      geometry = geometry, year = year, groups = 'all'
    )
  }

  if (geometry) {
    out <- make_planar_pair(out, epsg = epsg)$x
  }

  out
}


#' Create an ACS Tract-Level Table
#'
#' Download tract-level ACS data with the standard redistricting variables used
#' throughout the package.
#'
#' @param state Two-letter state postal code.
#' @param county Optional county name or code. If omitted, returns tracts for the
#'   entire state.
#' @param geometry Logical. If `TRUE`, include geometry.
#' @param year ACS year, currently intended for `2009` through `2019`.
#' @param mem Logical. If `TRUE`, use the memoized `censable` backend.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return dataframe or `sf` object with one row per tract. Includes population,
#'   voting-age population, and citizen voting-age population summaries by race
#'   and ethnicity.
#' @export
#'
#' @concept datatable
#' @examples \dontrun{
#' # Relies on Census Bureau API
#' tract <- create_tract_table('NY', 'Rockland', year = 2018)
#' }
create_tract_table <- function(state, county, geometry = TRUE, year = 2019,
                               mem = FALSE, epsg = 3857) {
  statepo <- censable::match_abb(state)

  if (length(statepo) == 0) {
    cli::cli_abort('Please provide a two letter postal abbreviation for the state.')
  }

  if (year < 2009 | year > 2019) {
    cli::cli_warn('Only years in 2009:2019 inclusive are currently supported.')
  }

  if (mem) {
    out <- censable::mem_build_acs(
      geography = 'tract', state = state, county = county,
      geometry = geometry, year = year, groups = 'all'
    )
  } else {
    out <- censable::build_acs(
      geography = 'tract', state = state, county = county,
      geometry = geometry, year = year, groups = 'all'
    )
  }

  if (geometry) {
    out <- make_planar_pair(out, epsg = epsg)$x
  }

  out
}

#' Aggregate a Block Table by Matches
#'
#' Aggregate block-level attributes up to a larger geography, usually precincts,
#' using a vector of group assignments such as the output of [geo_match()].
#'
#' @param block_table Block table, usually from [create_block_table()].
#' @param matches Integer grouping variable, typically from [geo_match()].
#' @param geometry Logical. If `TRUE`, union geometry within each matched group.
#'
#' @details
#' If `matches` carries a `"matching_max"` attribute, the output preserves all
#' target groups up to that size, even when some groups receive no matched rows.
#' Missing nonnegative numeric summaries are filled with `0`.
#'
#' @return dataframe with one row per matched group. When empty target groups are
#'   preserved, the number of rows may be larger than `length(unique(matches))`.
#' @export
#' @concept datatable
#' @examples
#' set.seed(1)
#' data(rockland)
#' rockland$id <- sample(c(1:2, 4), nrow(rockland), TRUE)
#' block2prec(rockland, rockland$id)
#'
block2prec <- function(block_table, matches, geometry = FALSE) {
  if (missing(block_table)) {
    cli::cli_abort('Please provide an argument to {.arg block_table}.')
  }
  if (missing(matches)) {
    cli::cli_abort('Please provide an argument to {.arg matches}.')
  }
  
  nrow_to <- attr(matches, 'matching_max')
  if (is.null(nrow_to)) {
    nrow_to <- max(matches)
  }

  block_table <- block_table |> 
    dplyr::mutate(matches_id = matches)

  if (!geometry) {
    ret <- block_table |>
      sf::st_drop_geometry() |>
      dplyr::group_by(matches_id) |>
      dplyr::summarize(dplyr::across(where(is.numeric), sum),
        dplyr::across(where(function(x) length(unique(x)) == 1), unique),
        .groups = 'drop'
      )
  } else {
    ret <- block_table |>
      dplyr::group_by(matches_id) |>
      dplyr::summarize(
        dplyr::across(where(is.numeric), sum),
        dplyr::across(where(function(x) length(unique(x)) == 1), unique),
        geometry = sf::st_union(geometry),
        .groups = 'drop'
      ) |>
      sf::st_as_sf()
  }

  if (nrow(ret) != nrow_to) {
    cols_to_fill <- lapply(ret, function(x) {
      if (is.numeric(x)) {
        if (all(x >= 0)) {
          # assume counts
          0L
        } else {
          # don't fill when there are negative values
         NA_integer_ 
        }
      } else if (length(unique(x)) == 1) {
        unique(x)
      } else {
        NA
      }
    }) |> 
      stats::setNames(names(ret)) |>
      Filter(f = function(x) !is.na(x))
      
    ret <- tidyr::complete(
      data = ret, matches_id = seq_len(nrow_to),
      fill = cols_to_fill,
      explicit = FALSE
    )
  }
  
  ret
}


#' Aggregate a Block Table by Matches Within County
#'
#' Variant of [block2prec()] that matches blocks to precincts separately within
#' each county. This helps avoid cross-county mismatches when county boundaries
#' and census blocks do not line up cleanly.
#'
#' @param block_table Block table, usually from [create_block_table()].
#' @param precinct `sf` object of target precinct shapes.
#' @param precinct_county_fips Name of the county identifier column in `precinct`.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return dataframe with one row per row of `precinct`, ordered to align with
#'   the input precinct object.
#' @export
#' @concept datatable
#' @examples \dontrun{
#' # Need Census API
#' data(towns)
#' towns$fips <- '087'
#' block <- create_block_table('NY', 'Rockland')
#' block2prec_by_county(block, towns, 'fips')
#' }
block2prec_by_county <- function(block_table, precinct, precinct_county_fips, epsg = 3857) {
  if (missing(block_table)) {
    cli::cli_abort('Please provide an argument to {.arg block_table}.')
  }
  if (missing(precinct)) {
    cli::cli_abort('Please provide an argument to {.arg precinct}.')
  }
  if (missing(precinct_county_fips)) {
    cli::cli_abort('Please provide an argument to {.argprecinct_county_fips}.')
  }
  if (!precinct_county_fips %in% names(precinct)) {
    cli::cli_abort('{.arg precinct_county_fips} is not the name of a column in precinct.')
  }

  pairs <- make_planar_pair(block_table, precinct, epsg = epsg)
  block_table <- pairs$x
  precinct <- pairs$y

  precinct <- precinct |>
    mutate(rowid = row_number()) |>
    select(rowid, geometry, all_of(precinct_county_fips))

  prectb <- tibble()
  countiesl <- unique(block_table$county)

  for (cty in seq_along(countiesl)) {
    bsub <- block_table |> filter(.data$county == countiesl[cty])
    psub <- precinct |>
      filter(.data[[precinct_county_fips]] == countiesl[cty]) |>
      mutate(matches_id = row_number())

    matches <- geo_match(from = bsub, to = psub, epsg = FALSE)
    prectemp <- block2prec(bsub, matches = matches)

    prectemp <- prectemp |>
      dplyr::left_join(y = psub |> sf::st_drop_geometry() |>
        dplyr::select(rowid, matches_id), by = 'matches_id')

    prectb <- prectb |>
      dplyr::bind_rows(prectemp)
  }

  prectb |>
    dplyr::arrange(rowid) |>
    dplyr::mutate(matches_id = rowid) |>
    dplyr::select(-rowid)
}


globalVariables(c(
  'GEOID', 'variable', 'value', 'AWATER10', 'ALAND10', 'County',
  'State', 'pop', 'pop_black', 'pop_hisp', 'pop_other',
  'pop_white', 'vap', 'rowid', 'geometry', '.data',
  'COUNTYFP', 'cvap_black', 'cvap_hisp', 'cvap_white', 'STATEFP',
  'estimate',
  'vap_black', 'vap_hisp', 'vap_other', 'vap_white', 'matches_id',
  'm_vap', 'm_nvap', 'f_vap', 'f_nvap', 'm_vap_black', 'Mnvap_black',
  'f_vap_black', 'f_nvap_black', 'm_vap_white', 'Mnvap_white', 'f_vap_white',
  'f_nvap_white', 'm_vap_hisp', 'Mnvap_hisp', 'f_vap_hisp', 'f_nvap_hisp',
  'where'
))
