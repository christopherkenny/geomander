#' Get Racially Polarized Voting Dataset from RPV Near Me
#'
#' @param state the state postal code of the state
#' @param version the version of the data to use. `1` for the original, `2` for the extended.
#'
#' @return a tibble of precinct-level estimates of votes (party) by race
#' @export
#'
#' @examples
#' get_rpvnearme('DE')
get_rpvnearme <- function(state, version = c(1, 2)) {
  
  cli::cli_inform(
    'Data sourced from the RPV Near Me {.url https://www.rpvnearme.org/}.',
    .frequency = 'once',
    .frequency_id = 'cite_rpvnearme'
  )
  
  vers <- match.arg(as.character(version), as.character(1:2))
  
  vers <- ifelse(vers == 1, '', '_b')
  
  if (missing(state)) {
    cli::cli_abort('Please provide a postal code, fips code, or name to state.')
  }
  
  state <- censable::match_abb(state)
  base_path <- stringr::str_glue(
    'https://raw.githubusercontent.com/electionlawclinic/rpvnearme/main/data/{state}_county_2020_precinct{vers}.csv'
  )
  
  readr::read_csv(base_path, show_col_types = FALSE)
}