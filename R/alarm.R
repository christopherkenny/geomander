#' Get ALARM Dataset
#'
#' Get's a dataset from the Algorithm-Assisted Redistricting Methodology Project.
#' The current supported data is the 2020 retabulations of the VEST data, which
#' can be downloaded with `get_vest`.
#' 
#' See the full available data at <https://github.com/alarm-redist/census-2020>.
#' 
#' @md
#' 
#'
#' @param state two letter state abbreviation
#' @param geometry Default is TRUE. Add geomeetry to the data?
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
#' ak <- get_alarm('AK')
#' }
get_alarm <- function(state, geometry = TRUE, epsg = 3857) {

  base_path <- 'https://raw.githubusercontent.com/alarm-redist/census-2020/main/census-vest-2020/'
  state <- tolower(censable::match_abb(state))
  block_states <- c('ca', 'hi', 'or')
  if (state %in% block_states) {
    end_path <- paste0(state, '_2020_block.csv')
  } else {
    end_path <- paste0(state, '_2020_vtd.csv')
  }

  tb <- readr::read_csv(file = paste0(base_path, end_path))

  if (geometry) {
    if (state  %in% block_states) {
      geo <- tigris::blocks(state = state, year = 2020)
    } else {
      geo <- tigris::voting_districts(state = state)
    }  
    
    geo <- geo %>%
      dplyr::select(.data$GEOID20, .data$geometry) %>%
      dplyr::mutate(GEOID20 = as.character(.data$GEOID20))
    tb <- tb %>%
      dplyr::mutate(GEOID20 = as.character(.data$GEOID20)) %>%
      dplyr::left_join(geo, by = 'GEOID20') %>%
      sf::st_as_sf()
    
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
