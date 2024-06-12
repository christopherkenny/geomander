#' Estimate Plans from a Block Assignment File to Voting Districts
#'
#' District lines are often provided at the census block level, but analyses
#' often occur at the voting district level. This provides a simple way to
#' estimate the block level to the voting district level.
#'
#' If a voting district is split between blocks, this currently uses the most
#' common district.
#'
#' @param baf a tibble representing a block assignment file.
#' @param plan_name character. Name of column in `baf` which corresponds to the districts.
#' @param GEOID character. Name of column which corresponds to each block's GEOID,
#' sometimes called "BLOCKID". Default is `'GEOID'`.
#' @param year the decade to request, either `2010` or `2020`. Default is `2020`.
#'
#' @return a tibble with a vtd-level assignment file
#' @export
#'
#' @examples
#' # Not guaranteed to reach download from redistrict2020.org
#' \dontrun{
#' # download and read baf ----
#' url <- paste0('https://github.com/PlanScore/Redistrict2020/', 
#'               'raw/main/files/DE-2021-01/DE_SLDU_bef.zip')
#' tf <- tempfile('.zip')
#' utils::download.file(url, tf)
#' utils::unzip(tf, exdir = dirname(tf))
#' baf <- readr::read_csv(
#'   file = paste0(dirname(tf), '/DE_SLDU_bef.csv'),
#'   col_types = 'ci'
#' )
#' names(baf) <- c('GEOID', 'ssd_20')
#'
#' # convert to vtd level ----
#' baf_to_vtd(baf = baf, plan_name = 'ssd_20', 'GEOID')
#' }
#'
baf_to_vtd <- function(baf, plan_name, GEOID = 'GEOID', year = 2020) {
  if (missing(baf)) {
    cli::cli_abort('{.arg baf} missing, but required.')
  }
  if (missing(plan_name)) {
    cli::cli_abort('{.arg plan_name} missing, but required.')
  }

  if (!inherits(baf, 'data.frame')) {
    cli::cli_abort('{.arg baf} must be a {.cls data.frame} or {.cls tibble}.')
  }
  if (!'character' %in% class(plan_name)) {
    cli::cli_abort('{.arg plan_name} must be a {.cls character}.')
  }
  if (!'character' %in% class(GEOID)) {
    cli::cli_abort('{.arg GEOID} must be a {.cls character}.')
  }
  
  if (!year %in% c(2010, 2020)) {
    cli::cli_abort('{.arg year} must be either 2010 or 2020.')
  }

  state_fips <- substr(baf[[GEOID]][1], 1, 2)

  baf_vtd <- baf::baf(state = state_fips, geographies = 'VTD')
  if (length(baf_vtd) == 0) {
    cli::cli_abort(c('VTD file not found for state {.val {state_fips}}.'))
  }
  baf_vtd <- baf_vtd[[1]] |>
    dplyr::rename(GEOID = .data$BLOCKID, county = .data$COUNTYFP, vtd = .data$DISTRICT)

  baf <- baf |>
    dplyr::rename(GEOID = .env$GEOID) |>
    left_join(baf_vtd, by = 'GEOID')

  baf |>
    dplyr::select(-.data$GEOID) |>
    dplyr::mutate(GEOID = paste0(state_fips, .data$county, .data$vtd)) |>
    dplyr::select(-.data$county, .data$vtd) |>
    dplyr::group_by(.data$GEOID) |>
    dplyr::summarize({{ plan_name }} := as.integer(Mode(.data[[plan_name]])))
}

Mode <- function(v) {
  uv <- unique(v)
  uv[which.max(tabulate(match(v, uv)))][1]
}
