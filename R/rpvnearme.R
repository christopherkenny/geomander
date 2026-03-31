#' Get the RPV Near Me Dataset
#'
#' Download precinct-level racially polarized voting estimates from the RPV Near
#' Me repository.
#'
#' @param state State postal code.
#' @param version Dataset version. Use `1` for the original release or `2` for
#'   the extended release.
#'
#' @return tibble of precinct-level estimates of party vote by race
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
