#' Get a Voting and Election Science Team ("VEST") Dataset
#'
#' Download a state-year VEST precinct file from Harvard Dataverse.
#'
#' @param state Two-letter state abbreviation.
#' @param year Election year, currently one of `2016` through `2021`.
#' @param path Directory used for extracted files. Defaults to `tempdir()`.
#' @param clean_names Logical. If `TRUE`, rename election columns into a more
#'   consistent scheme.
#' @param epsg `r roxy_epsg()`
#' @param ... Additional arguments passed to [sf::read_sf()].
#'
#' @return `sf` tibble containing the requested VEST dataset
#' @export
#'
#' @concept datasets
#' @examples
#' \dontrun{
#' # Requires Dataverse API
#' shp <- get_vest('CO', 2020)
#' }
get_vest <- function(state, year, path = tempdir(), clean_names = TRUE, epsg = 3857, ...) {
  abb <- tolower(censable::match_abb(state))

  file_name <- stringr::str_glue('{abb}_{year}.zip')
  if (state %in% c('fl', 'md', 'nj', 'tx') && year == 2016) {
    file_name <- stringr::str_glue('{abb}.zip')
  }

  doi <- vest_doi()[as.character(year)]
  cite_url <- paste0('https://doi.org/', stringr::str_sub(doi, 5))
  cli::cli_inform(
    'Data sourced from the Voting and Election Science Team {.url {cite_url}}.',
    .frequency = 'once',
    .frequency_id = 'cite_vest'
  )

  tf <- tempfile(fileext = '.zip')
  x <- dataverse::get_file_by_name(
    filename = file_name, dataset = doi,
    server = 'dataverse.harvard.edu'
  ) |>
    writeBin(con = tf)
  utils::unzip(tf, exdir = path)

  poss <- sf::st_layers(dsn = path)[[1]]
  up_path <- poss[stringr::str_starts(string = poss, stringr::str_glue('{abb}_{year}'))]

  out <- sf::read_sf(dsn = paste0(path, '/', up_path, '.shp'), ...)

  if (clean_names) {
    out <- out |> clean_vest()
  }

  make_planar_pair(out, epsg = epsg)$x
}

#' Clean VEST Column Names
#'
#' Rename VEST election-result columns into a more uniform
#' `office_year_party_candidate` style.
#'
#' @param data `sf` tibble from [get_vest()].
#'
#' @return input object with renamed columns
#' @export
#'
#' @concept datasets
#' @examples
#' data(va18sub)
#' va <- clean_vest(va18sub)
clean_vest <- function(data) {
  noms <- names(data)

  gen <- grep('G[0-9]{2}', noms, value = FALSE) # General
  run <- grep('R[0-9]{2}', noms, value = FALSE) # Runoff; necessary for LA/GA/etc

  for (i in seq_along(gen)) {
    off <- tolower(stringr::str_sub(noms[gen[i]], 4, 6))
    yr <- stringr::str_sub(noms[gen[i]], 2, 3)
    party <- vest_party(noms[gen[i]])
    cand <- tolower(stringr::str_sub(noms[gen[i]], 8, 10))
    noms[gen[i]] <- stringr::str_glue('{off}_{yr}_{party}_{cand}')
  }

  for (i in seq_along(run)) {
    off <- tolower(stringr::str_sub(noms[run[i]], 4, 6))
    yr <- paste0('r', stringr::str_sub(noms[run[i]], 2, 3))
    party <- vest_party(noms[run[i]])
    cand <- tolower(stringr::str_sub(noms[run[i]], 8, 10))
    noms[run[i]] <- stringr::str_glue('{off}_{yr}_{party}_{cand}')
  }

  names(data) <- noms

  data
}

#' List Available VEST States
#'
#' @param year Year to inspect in the VEST Dataverse collection.
#'
#' @return character vector of state abbreviations available for that year
#' @export
#'
#' @concept datasets
#' @examples
#' \dontrun{
#' # Requires Dataverse API
#' vest_states(2020)
#' }
vest_states <- function(year) {
  doi <- vest_doi()[as.character(year)]
  files <- dataverse::dataset_files(doi, server = 'dataverse.harvard.edu')
  out <- stringr::str_sub(unlist(lapply(files, function(x) {
    x[['label']]
  })), 1, 2)
  base::setdiff(out, 'do')
}

#' VEST DOIs
#' @keywords internal
vest_doi <- function() {
  c(
    `2021` = 'doi:10.7910/DVN/FDMI5F',
    `2020` = 'doi:10.7910/DVN/K7760H',
    `2019` = 'doi:10.7910/DVN/2AJUII',
    `2018` = 'doi:10.7910/DVN/UBKYRU',
    `2017` = 'doi:10.7910/DVN/VNJAB1',
    `2016` = 'doi:10.7910/DVN/NH5S2I'
  )
}

#' VEST Parties
#' @keywords internal
vest_party <- function(str) {
  p <- stringr::str_sub(str, 7, 7)
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
  } else if (p == 'I') {
    p <- 'ind'
  } else if (p == 'C') {
    p <- 'con'
  } else {
    p <- 'unk'
  }

  p
}

#' VEST Abbreviations
#' @keywords internal
vest_abb <- function(x) {
  structure(
    list(
      a = c(
        'A##', 'AGR', 'ATG', 'AUD', 'CFO',
        'CHA', 'COC', 'COM', 'CON', 'COU', 'CSC', 'DEL', 'GOV',
        'H##', 'HOD', 'HOR', 'INS', 'LAB', 'LND', 'LTG', 'MAY',
        'MNI', 'PSC', 'PUC', 'RGT', 'SAC', 'SBE', 'SCC', 'SOC',
        'SOS', 'SPI', 'SPL', 'SSC', 'TAX', 'TRE', 'UBR', 'USS'
      ),
      b = c(
        'Ballot amendment, where ## is an identifier', 'Commissioner of Agriculture',
        'Attorney General', 'Auditor', 'Chief Financial Officer',
        'Council Chairman', 'Corporation Commissioner', 'Comptroller',
        'State Controller', 'City Council Member', 'Clerk of the Supreme Court',
        'Delegate to the U.S. House', 'Governor', 'U.S. House, where ## is the district number. AL: at large.',
        'House of Delegates, accompanied by a HOD_DIST column indicating district number',
        'U.S. House, accompanied by a HOR_DIST column indicating district number',
        'Insurance Commissioner', 'Labor Commissioner', 'Commissioner of Public/State Lands',
        'Lieutenant Governor', 'Mayor', 'State Mine Inspector', 'Public Service Commissioner',
        'Public Utilities Commissioner', 'State University Regent',
        'State Appeals Court (in AL: Civil Appeals)', 'State Board of Education',
        'State Court of Criminal Appeals', 'Secretary of Commonwealth',
        'Secretary of State', 'Superintendent of Public Instruction',
        'Commissioner of School and Public Lands', 'State Supreme Court',
        'Tax Commissioner', 'Treasurer', 'University Board of Regents/Trustees/Governors',
        'U.S. Senate'
      )
    ),
    row.names = c(NA, -37L),
    class = c(
      'tbl_df',
      'tbl', 'data.frame'
    )
  )
}
