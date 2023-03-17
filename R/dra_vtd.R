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
#' @templateVar epsg TRUE
#' @template template
#'
#' @return tibble with election data and optional geometry
#' @export
#'
#' @concept datasets
#' @examples
#' \donttest{
#' # Takes a few seconds to run
#' ak <- get_dra('AK')
#' }
get_dra <- function(state, year = 2020, geometry = TRUE, epsg = 3857) {
  cli::cli_inform(
    'Data sourced from Dave\'s Redistricting {.url https://github.com/dra2020/vtd_data}.',
    .frequency = 'once',
    .frequency_id = 'cite_dra'
  )
  
  base_path <- stringr::str_glue(
    'https://raw.githubusercontent.com/dra2020/vtd_data/master/{year}_VTD/'
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
  end_path <-stringr::str_glue('{state}/{year}_election_{state}.csv')
  
  tb <- readr::read_csv(file = paste0(base_path, end_path), col_types = spec, 
                        show_col_types = FALSE)
  
  
  tb
}