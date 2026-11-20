#' Read a Redistricting Data Hub ("RDH") Dataset
#'
#' Read a state-year RDH precinct file.
#'
#' @param file Path to an RDH file, either zipped or a file readable by
#'   [sf::read_sf()], such as a `.shp`.
#' @param path Directory used for extracted files. Defaults to `tempdir()`.
#' @param clean_names Logical. If `TRUE`, rename election columns into a more
#'   consistent scheme.
#' @param epsg `r roxy_epsg()`
#' @param ... Additional arguments passed to [sf::read_sf()].
#'
#' @return `sf` tibble containing the RDH dataset
#' @export
#'
#' @concept datasets
#' @examples
#' \dontrun{
#' shp <- read_rdh('in_2024_gen_prec.zip')
#' }
read_rdh <- function(
  file,
  path = tempdir(),
  clean_names = TRUE,
  epsg = 3857,
  ...
) {
  if (stringr::str_detect(file, '(?i)\\.zip$')) {
    poss <- utils::unzip(file, list = TRUE)
    poss <- poss |>
      dplyr::filter(
        !stringr::str_detect(.data$Name, 'MACOSX'),
        stringr::str_detect(.data$Name, '\\.(shp|geojson|gpkg)$')
      )
    if (nrow(poss) > 1) {
      poss <- poss |>
        dplyr::filter(stringr::str_detect(.data$Name, '_all_'))
    }
    if (nrow(poss) != 1) {
      cli::cli_abort(
        'Expected exactly one spatial file in {.file {file}}, found {nrow(poss)}.'
      )
    }
    utils::unzip(file, exdir = path)
    file <- paste0(path, '/', poss$Name[1])
  }

  out <- sf::read_sf(dsn = file, ...)

  if (clean_names) {
    out <- out |>
      clean_rdh()
  }

  make_planar_pair(out, epsg = epsg)$x
}

#' Clean RDH Column Names
#'
#' Rename RDH election-result columns into a more uniform
#' `office_year_party_candidate` style.
#'
#' @param data `sf` tibble from [read_rdh()].
#' @param year Election year for district columns without a year, like
#'   `GCON01DMRV`. If `NULL`, inferred from the other columns.
#'
#' @return input object with renamed columns
#' @export
#'
#' @concept datasets
#' @examples
#' \dontrun{
#' shp <- read_rdh('in_2024_gen_prec.zip', clean_names = FALSE)
#' shp <- clean_rdh(shp)
#' }
clean_rdh <- function(data, year = NULL) {
  noms <- names(data)

  # ballot questions
  que <- grep(
    '^[GPRS](\\d{2}[A-Z]+\\d*|[A-Z]+\\d+)(YES|NO)$',
    noms
  )
  # candidates
  can <- grep(
    '^[GPRS](\\d{2}[A-Z]+\\d*|[A-Z]+\\d+)[A-Z]{4}$',
    noms
  )
  que <- base::setdiff(que, can)
  # paired YES/NO, FOR/AGA, or Y/N columns are always questions
  yn <- grep('^[GPRS](\\d{2})?[A-Z]+\\d*[A-Z]*(YES|NO|FOR|AGA|Y|N)$', noms)
  stem <- sub('(YES|NO|FOR|AGA|Y|N)$', '', noms[yn])
  que <- base::union(que, yn[stem %in% stem[duplicated(stem)]])
  can <- base::setdiff(can, que)

  if (is.null(year) || is.na(year)) {
    year <- stringr::str_sub(
      grep('^[GPRS][0-9]{2}', noms, value = TRUE)[1],
      2,
      3
    )
  }
  year <- stringr::str_sub(year, -2, -1)
  if (is.na(year) && any(grepl('^[GPRS][A-Z]', noms[c(can, que)]))) {
    cli::cli_abort(
      'Could not infer the election year. Supply it with {.arg year}.'
    )
  }

  for (i in seq_along(can)) {
    mt <- stringr::str_match(
      noms[can[i]],
      '^([GPRS])([0-9]{2})?([A-Z]+?)([0-9]*)([A-Z])([A-Z]{3})$'
    )
    off <- tolower(mt[4])
    off <- ifelse(
      is.na(rdh_abb_to_alarm[off]) || (off == 'con' && mt[5] == ''),
      off,
      rdh_abb_to_alarm[off]
    )
    off <- paste0(off, mt[5])
    yr <- ifelse(is.na(mt[3]), year, mt[3])
    yr <- ifelse(mt[2] == 'G', yr, paste0(tolower(mt[2]), yr))
    party <- rdh_party(mt[6])
    cand <- tolower(mt[7])
    noms[can[i]] <- stringr::str_glue('{off}_{yr}_{party}_{cand}')
  }

  for (i in seq_along(que)) {
    mt <- stringr::str_match(
      noms[que[i]],
      '^([GPRS])(\\d{2})?([A-Z]+)(\\d*)([A-Z]*)(YES|NO|FOR|AGA|Y|N)$'
    )
    off <- tolower(mt[4])
    off <- ifelse(
      is.na(rdh_abb_to_alarm[off]),
      off,
      rdh_abb_to_alarm[off]
    )
    off <- paste0(off, mt[5], tolower(mt[6]))
    yr <- ifelse(is.na(mt[3]), year, mt[3])
    yr <- ifelse(mt[2] == 'G', yr, paste0(tolower(mt[2]), yr))
    resp <- switch(
      tolower(mt[7]),
      'for' = 'yes',
      y = 'yes',
      aga = 'no',
      n = 'no',
      tolower(mt[7])
    )
    noms[que[i]] <- stringr::str_glue('{off}_{yr}_{resp}')
  }

  names(data) <- noms

  data
}

#' RDH Parties
#' @keywords internal
rdh_party <- function(str) {
  p <- str
  if (p == 'R') {
    p <- 'rep'
  } else if (p == 'D') {
    p <- 'dem'
  } else if (p == 'L') {
    p <- 'lib'
  } else if (p == 'G') {
    p <- 'gre'
  } else if (p == 'O') {
    p <- 'oth'
  } else if (p == 'I') {
    p <- 'ind'
  } else if (p == 'C') {
    p <- 'con'
  } else if (p == 'N') {
    p <- 'non'
  } else if (p == 'U') {
    p <- 'una'
  } else {
    p <- 'unk'
  }

  p
}

# fmt: skip
rdh_abb_to_alarm <- tibble::tribble(
  ~rdh, ~alarm,
  'PRE', 'pre',
  'USS', 'uss',
  'CON', 'ush',
  'GOV', 'gov',
  'LTG', 'ltg',
  'ATG', 'atg',
  'SOS', 'sos',
  'TRE', 'tre',
  'AUD', 'aud',
  'INS', 'ins',
  'SPI', 'spi',
  'SU', 'ssd',
  'SL', 'shd',
  'J', 'jud',
  'Q', 'que'
) |>
  dplyr::mutate(rdh = tolower(.data$rdh)) |>
  tibble::deframe()
