#' Get Dave's Redistricting App Dataset
#'
#' Gets a dataset from Dave's Redistricting App.
#'
#' See the full available data at <https://github.com/dra2020/vtd_data>.
#'
#'
#' @param state two letter state abbreviation
#' @param year year to get data for. Either `2020` or `2010`
#' @param geometry Default is TRUE. Add geometry to the data?
#' @param clean_names Clean names. Default is \code{TRUE}. If \code{FALSE},
#' returns default names.
#' @param epsg `r roxy_epsg()`
#'
#' @return tibble with election data and optional geometry
#' @export
#'
#' @concept datasets
#' @examples
#' ak <- get_dra('AK', geometry = FALSE)
get_dra <- function(state, year = 2020, geometry = TRUE, clean_names = TRUE, epsg = 3857) {
  cli::cli_inform(
    'Data sourced from Dave\'s Redistricting {.url https://github.com/dra2020/vtd_data}.',
    .frequency = 'once',
    .frequency_id = 'cite_dra'
  )

  base_path <- stringr::str_glue(
    'https://raw.githubusercontent.com/dra2020/vtd_data/d8550e99c3ed5160d1851e21df321fd7be4dca70/{year}_VTD/'
  )
  state <- toupper(censable::match_abb(state))
  block_group_states_2020 <- c('CA', 'HI', 'OR', 'WV')
  # ME 2020 needs to be vtd + cousubs...
  block_group_states_2010 <- c('CA', 'MT', 'OR', 'RI')
  if (year == 2020) {
    spec <- readr::cols(GEOID20 = 'c', .default = 'd')
  } else {
    spec <- readr::cols(GEOID10 = 'c', .default = 'd')
  }
  end_path <- stringr::str_glue('{state}/{year}_election_{state}.csv')

  tb <- readr::read_csv(
    file = paste0(base_path, end_path), col_types = spec,
    show_col_types = FALSE
  ) %>%
    dplyr::rename_with(.fn = \(x) stringr::str_remove_all(x, pattern = '\\d+'), .cols = dplyr::starts_with('GEOID'))

  if (geometry) {
    if ((year == 2020 && !state %in% block_group_states_2020 && state != 'ME') ||
      (year == 2010 && !state %in% block_group_states_2010)) {
      # regular vtds
      geo <- tinytiger::tt_voting_districts(state = state, year = year) %>%
        dplyr::select(dplyr::any_of(c(GEOID = 'GEOID10', GEOID = 'GEOID20')), 'geometry') %>%
        dplyr::mutate(dplyr::across('GEOID', as.character))
    } else {
      if (year == 2020 && state == 'ME') {
        # frankenstein's vtds
        geo_a <- tinytiger::tt_voting_districts(state = state, year = year) %>%
          dplyr::select(dplyr::any_of(c(GEOID = 'GEOID20', county = 'COUNTYFP20')), 'geometry') %>%
          dplyr::mutate(dplyr::across('GEOID', as.character))
        geo_b <- tinytiger::tt_county_subdivisions(state = state, year = year) %>%
          dplyr::select(dplyr::any_of(c('GEOID', county = 'COUNTYFP')), 'geometry') %>%
          dplyr::mutate(dplyr::across('GEOID', as.character))
        geo <- dplyr::bind_rows(
          geo_a, geo_b %>% dplyr::filter(!.data$county %in% geo_a$county)
        )
      } else {
        # block groups
        geo <- tinytiger::tt_block_groups(state = state, year = year) %>%
          dplyr::select(dplyr::any_of(c(GEOID = 'GEOID10', GEOID = 'GEOID20')), 'geometry') %>%
          dplyr::mutate(dplyr::across('GEOID', as.character))
      }
    }
    tb <- tb %>%
      dplyr::left_join(geo, by = 'GEOID') %>%
      dplyr::relocate('GEOID', .before = dplyr::everything()) %>%
      sf::st_as_sf()

    tb <- make_planar_pair(tb, epsg = epsg)$x
  }

  if (clean_names) {
    tb <- clean_dra(tb)
  }

  tb
}

#' Clean DRA Names
#'
#' @param data sf tibble from DRA
#'
#' @return data with cleaned names
#' @noRd
clean_dra <- function(data) {
  noms <- names(data)

  gen <- grep('[0-9]{4}', noms, value = FALSE) # General
  run <- which(stringr::str_ends(string = noms, 'roff')) # Runoff; necessary for LA/GA/etc
  spe <- which(stringr::str_ends(string = noms, 'spec')) # Special
  # spr <- which(stringr::str_ends(string = noms, 'sproff'))


  for (i in seq_along(gen)) {
    off <- dra_office[tolower(stringr::word(noms[gen[i]], 3, sep = stringr::fixed('_')))]
    yr <- stringr::str_sub(stringr::str_extract(noms[gen[i]], '\\d+'), start = -2L)
    party <- dra_party(noms[gen[i]])
    noms[gen[i]] <- stringr::str_glue('{off}_{yr}_{party}')
  }

  for (i in seq_along(run)) {
    off <- dra_office[tolower(stringr::word(noms[gen[i]], 3, sep = stringr::fixed('_')))]
    yr <- paste0('r', stringr::str_sub(stringr::str_extract(noms[gen[i]], '\\d+'), start = -2L))
    party <- dra_party(noms[gen[i]])
    noms[run[i]] <- stringr::str_glue('{off}_{yr}_{party}')
  }

  for (i in seq_along(spe)) {
    off <- dra_office[tolower(stringr::word(noms[gen[i]], 3, sep = stringr::fixed('_')))]
    yr <- paste0('s', stringr::str_sub(stringr::str_extract(noms[gen[i]], '\\d+'), start = -2L))
    party <- dra_party(noms[gen[i]])
    noms[run[i]] <- stringr::str_glue('{off}_{yr}_{party}')
  }

  # for (i in seq_along(spr)) {
  #   off <- dra_office[tolower(stringr::word(noms[gen[i]], 3, sep = stringr::fixed('_')))]
  #   yr <- paste0('s', stringr::str_sub(stringr::str_extract(noms[gen[i]], '\\d+'), start = -2L))
  #   party <- dra_party(noms[gen[i]])
  #   noms[run[i]] <- stringr::str_glue('{off}_{yr}_{party}')
  # }

  names(data) <- noms

  data
}

#' DRA Parties
#' @keywords internal
#' @noRd
dra_party <- function(str) {
  p <- stringr::word(str, sep = stringr::fixed('_'))
  if (p %in% c('R', 'Rep')) {
    p <- 'rep'
  } else if (p %in% c('D', 'Dem')) {
    p <- 'dem'
  } else if (p == 'Tot') {
    p <- 'tot'
  } else {
    p <- 'unk'
  }

  p
}

#' DRA Offices
#' @keywords internal
#' @noRd
dra_office <- tibble::tribble(
  ~dra, ~alarm,
  'gov', 'gov',
  'ag', 'atg',
  'ltg', 'ltg',
  'pres', 'pre',
  'sen', 'uss'
) %>%
  tibble::deframe()
