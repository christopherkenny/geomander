#' Get ALARM Dataset
#'
#' Gets a dataset from the Algorithm-Assisted Redistricting Methodology Project.
#' The current supported data is the 2020 retabulations of the VEST data, which
#' can be downloaded with `get_vest`.
#'
#' See the full available data at <https://github.com/alarm-redist/census-2020>.
#'
#'
#' @param state two letter state abbreviation
#' @param year year to get data for. Either `2020` or `2010`
#' @param geometry Default is TRUE. Add geometry to the data?
#' @templateVar epsg TRUE
#' @template template
#'
#' @return tibble with election data and optional geometry
#' @export
#'
#' @concept datasets
#' @examples
#' ak <- get_alarm('AK', geometry = FALSE)
get_alarm <- function(state, year = 2020, geometry = TRUE, epsg = 3857) {
  cli::cli_inform(
    'Data sourced from the ALARM Project {.url https://github.com/alarm-redist/census-2020}.',
    .frequency = 'once',
    .frequency_id = 'cite_alarm'
  )

  base_path <- stringr::str_glue(
    'https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-{year}/'
  )
  state <- tolower(censable::match_abb(state))
  block_states_2020 <- c('ca', 'hi', 'or') # TODO: ME probably due to non-coverage?
  block_states_2010 <- c('ca', 'hi', 'ky', 'or', 'ri', 'wa')
  is_block <- FALSE
  if (year == 2020) {
    spec <- readr::cols(GEOID20 = 'c', state = 'c', county = 'c', vtd = 'c', .default = 'd')
    if (state %in% block_states_2020) {
      end_path <- paste0(state, '_2020_block.csv')
      spec[[1]]$vtd <- NULL
      is_block <- TRUE
    } else {
      end_path <- paste0(state, '_2020_vtd.csv')
    }
  } else {
    spec <- readr::cols(state = 'c', county = 'c', vtd = 'c', .default = 'd')
    if (state %in% block_states_2010) {
      end_path <- paste0(state, '_2010_block.csv')
      spec[[1]]$vtd <- NULL
      is_block <- TRUE
    } else {
      end_path <- paste0(state, '_2010_vtd.csv')
    }
  }


  tb <- readr::read_csv(
    file = paste0(base_path, end_path), col_types = spec,
    show_col_types = FALSE
  )

  if (geometry) {
    if ((state %in% block_states_2020 && year == 2020) || (state %in% block_states_2010 && year == 2010)) {
      geo <- tinytiger::tt_blocks(state = state, year = year)
    } else {
      geo <- tinytiger::tt_voting_districts(state = state, year = year)
    }

    if (year == 2020) {
      geo <- geo %>%
        dplyr::select(.data$GEOID20, .data$geometry) %>%
        dplyr::mutate(
          GEOID20 = as.character(.data$GEOID20),
          .before = dplyr::everything()
        )
      tb <- tb %>%
        dplyr::mutate(
          GEOID20 = as.character(.data$GEOID20),
          .before = dplyr::everything()
        ) %>%
        dplyr::left_join(geo, by = 'GEOID20') %>%
        sf::st_as_sf()
    } else if (year == 2010) {
      geo <- geo %>%
        dplyr::select(.data$GEOID10, .data$geometry) %>%
        dplyr::mutate(
          GEOID10 = as.character(.data$GEOID10),
          .before = dplyr::everything()
        )
      if (is_block) {
        tb <- tb %>%
          dplyr::mutate(
            GEOID10 = paste0(.data$state, .data$county, .data$tract, .data$block),
            .before = dplyr::everything()
          ) %>%
          dplyr::left_join(geo, by = 'GEOID10') %>%
          sf::st_as_sf()
      } else {
        tb <- tb %>%
          dplyr::mutate(
            GEOID10 = paste0(.data$state, .data$county, .data$vtd),
            .before = dplyr::everything()
          ) %>%
          dplyr::left_join(geo, by = 'GEOID10') %>%
          sf::st_as_sf()
      }
    }

    tb <- make_planar_pair(tb, epsg = epsg)$x
  }

  tb
}


#' List Available States from ALARM Data
#'
#' @return character abbreviations for states
#' @export
#'
#' @concept datasets
#' @examples
#' \dontrun{
#' # relies on internet availability and interactivity on some systems
#' alarm_states()
#' }
alarm_states <- function() {
  con <- 'https://api.github.com/repos/christopherkenny/census-2020/git/trees/813facc1aca4b5d6a84bdae3efcf61b953d633cb'
  tf <- tempfile(fileext = '.json')
  x <- utils::download.file(url = con, tf)
  files <- readChar(tf, nchars = 1e5)
  files <- stringr::str_split(files, '"', simplify = TRUE)
  files <- files[grep('.csv', files)]
  stringr::str_sub(files, 1, 2)
}
