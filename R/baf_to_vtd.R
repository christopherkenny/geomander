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
#'
#' @return a tibble with a vtd-level assignment file
#' @export
#'
#' @examples
#' # Not guaranteed to reach download from redistrict2020.org
#' \dontrun{
#' # download and read baf ----
#' url <- 'https://www.redistrict2020.org/files/DE-2021-01/DE_SLDU_bef.zip'
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
baf_to_vtd <- function(baf, plan_name, GEOID = 'GEOID') {
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

  state_fips <- substr(baf[[GEOID]][1], 1, 2)

  baf_vtd <- download_2020_vtd_baf(state = state_fips)

  baf <- baf %>%
    dplyr::rename(GEOID = .env$GEOID) %>%
    left_join(baf_vtd, by = 'GEOID')

  baf %>%
    dplyr::select(-.data$GEOID) %>%
    dplyr::mutate(GEOID = paste0(state_fips, .data$county, .data$vtd)) %>%
    dplyr::select(-.data$county, .data$vtd) %>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::summarize({{ plan_name }} := as.integer(Mode(.data[[plan_name]])))
}

# adapted & simplified from PL94171::pl_get_baf()
download_2020_vtd_baf <- function(state) {
  fips <- censable::match_fips(state)
  abb <- censable::match_abb(state)

  zip_url <- paste0(
    'https://www2.census.gov/geo/docs/maps-data/data/baf2020/BlockAssign_ST',
    fips, '_', abb, '.zip'
  )
  tf <- tempfile(fileext = '.zip')
  utils::download.file(zip_url, tf)
  baf_vtd_path <- utils::unzip(
    zipfile = tf,
    files = paste0('BlockAssign_ST', fips, '_', abb, '_VTD.txt'),
    exdir = dirname(tf)
  ) %>%
    suppressWarnings()

  if (length(baf_vtd_path) == 0) {
    cli::cli_abort(c('VTD file not found in zip file.',
      'i' = paste0('Check downloaded files at ', tf, '.')
    ))
  }

  readr::read_delim(
    file = baf_vtd_path,
    delim = '|',
    col_types = readr::cols(.default = 'c'),
    progress = interactive()
  ) %>%
    dplyr::rename(GEOID = .data$BLOCKID, county = .data$COUNTYFP, vtd = .data$DISTRICT)
}

Mode <- function(v) {
  uv <- unique(v)
  uv[which.max(tabulate(match(v, uv)))][1]
}
