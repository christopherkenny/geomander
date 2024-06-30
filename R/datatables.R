#' Create Block Level Data
#'
#' Creates a block level dataset, using the decennial census information, with the
#' standard redistricting variables.
#'
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns blocks for the entire state.
#' @param geometry Defaults to TRUE. Whether to return the geometry or not.
#' @param year year, must be 2000, 2010, or 2020
#' @param mem Default is FALSE. Set TRUE to use memoized backend.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return dataframe with data for each block in the selected region. Data includes
#' 2 sets of columns for each race or ethnicity category: population (pop) and
#' voting age population (vap)
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


#' Create Tract Level Data
#'
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns tracts for the entire state.
#' @param geometry Defaults to TRUE. Whether to return the geography or not.
#' @param year year, must be >= 2009 and <= 2019.
#' @param mem Default is FALSE. Set TRUE to use memoized backend.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return dataframe with data for each tract in the selected region. Data includes
#' 3 sets of columns for each race or ethnicity category: population (pop), voting age
#' population (vap), and citizen voting age population (cvap)
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

#'  Aggregate Block Table by Matches
#'
#' Aggregates block table values up to a higher level, normally precincts, hence
#' the name block2prec.
#'
#' @param block_table Required. Block table output from create_block_table
#' @param matches Required. Grouping variable to aggregate up by, typically made with geo_match
#' @param geometry Boolean. Whether to keep geometry or not.
#'
#' @return dataframe with length(unique(matches)) rows
#' @export
#' @concept datatable
#' @examples
#' set.seed(1)
#' data(rockland)
#' rockland$id <- sample(1:2, nrow(rockland), TRUE)
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
      purrr::set_names(names(ret)) |>
      purrr::discard(function(x) is.na(x))
      
    ret <- tidyr::complete(
      data = ret, matches_id = seq_len(nrow_to),
      fill = cols_to_fill,
      explicit = FALSE
    )
  }
  
  ret
}


#' Aggregate Block Table by Matches and County
#'
#' Performs the same type of operation as block2prec, but subsets a precinct geometry
#' based on a County fips column. This helps get around the problem that county geometries
#' often have borders that follow rivers and lead to funny shaped blocks. This guarantees
#' that every block is matched to a precinct which is in the same county.
#'
#' @param block_table Required. Block table output from create_block_table
#' @param precinct sf dataframe of shapefiles to match to.
#' @param precinct_county_fips Column within precincts
#' @templateVar epsg TRUE
#' @template template
#'
#' @return dataframe with nrow(precinct) rows
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
