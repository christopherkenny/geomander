#' Convert DRA Export Data to an R Spatial Object
#'
#' Read a DRA block assignment export and attach it to 2020 census blocks,
#' optionally collapsing the assignments to a precinct layer.
#'
#' @param dra Path to a DRA CSV export, or a dataframe containing columns
#'   `GEOID20` and `District`.
#' @param state State postal abbreviation used to fetch 2020 blocks.
#' @param precincts Optional `sf` object of precinct shapes. If supplied, the
#'   function assigns each precinct the most common block-level district among
#'   the blocks matched to it.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return `sf` object at the block level when `precincts` is omitted, or at the
#'   precinct level when `precincts` is supplied.
#' @export
#' @concept dra
#' @examples\dontrun{
#' # Needs Census Bureau API
#' # dra_utah_test is available at https://bit.ly/3c6UDKk
#' blocklevel <- dra2r('dra_utah_test.csv', state = 'UT')
#' }
dra2r <- function(dra, state, precincts, epsg = 3857) {
  # check inputs
  if (missing(dra)) {
    cli::cli_abort('Please provide an argument to {.arg dra}.')
  }
  if (missing(state)) {
    cli::cli_abort('Please provide a postal code, fips code, or name to state.')
  }

  # make dra into a dataframe
  if ('character' %in% class(dra)) {
    # check that it's pointing to a good thing:
    if (!file.exists(dra)) {
      cli::cli_abort('{.arg dra} supplied as character, but does not point to a valid file.')
    }

    # get the file extension
    ext <- stringr::str_sub(dra, start = -4)
    if (ext == '.csv') {
      dra <- readr::read_csv(file = dra, col_types = c(GEOID20 = 'c', District = 'c'), col_names = TRUE)
    } else if (ext == 'json') {
      cli::cli_abort('Please export the map as a csv. json not currently supported, but may be in the future.')
    } else {
      cli::cli_abort('dra points to a valid file, but not a csv. Please point to a csv export of the map.')
    }


    if (!('GEOID20' %in% names(dra) & 'District' %in% names(dra))) {
      cli::cli_abort('{.arg dra} points to a file where {.var GEOID20} and/or {.var District} column not present.')
    }
  } else {
    if (!('GEOID20' %in% names(dra) & 'District' %in% names(dra))) {
      cli::cli_abort('{.arg dra} provided as dataframe, but {.var GEOID20} and/or {.var District} column not present.')
    }
  }

  # rename District column
  dra <- dra |> dplyr::rename(District_DRA = District)

  # get the block file to match it to
  shp <- tinytiger::tt_blocks(state, year = 2020)

  # join them together
  shp <- shp |> dplyr::left_join(dra, by = 'GEOID20')

  # match to precincts if provided
  if (!missing(precincts)) {
    pairs <- make_planar_pair(precincts, shp)
    precincts <- pairs$x
    shp <- pairs$y

    matches <- geo_match(from = shp, to = precincts, epsg = FALSE)

    shp <- shp |> dplyr::mutate(matches = matches)

    shp <- shp |>
      sf::st_drop_geometry() |>
      dplyr::select(matches, District_DRA)

    # get the most likely match
    shp <- shp |>
      dplyr::group_by(matches, District_DRA) |>
      dplyr::mutate(n = n()) |>
      dplyr::ungroup() |>
      dplyr::group_by(matches) |>
      dplyr::arrange(desc(n)) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(-n)

    precincts <- precincts |>
      dplyr::left_join(shp, by = 'matches') |>
      dplyr::select(-matches)

    return(precincts)
  }

  make_planar_pair(shp, epsg = epsg)$x
}

#' Convert an R Plan to DRA Block Assignment Format
#'
#' Project a plan defined on precincts down to 2020 census blocks and return the
#' two-column format expected by DRA exports.
#'
#' @param precincts `sf` object of precinct geometries.
#' @param plan Either a vector of district assignments aligned with `precincts`,
#'   or the name of a column in `precincts` containing those assignments.
#' @param state State postal abbreviation used to fetch 2020 blocks.
#' @param path Optional output path. If supplied, the result is written as CSV and
#'   also returned.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return tibble with columns `GEOID20` and `District`, suitable for writing to
#'   a DRA-style CSV.
#' @export
#' @concept dra
#' @examples \dontrun{
#' # Needs Census Bureau API
#' cd <- tinytiger::tt_congressional_districts() |> filter(STATEFP == '49')
#' cnty <- tinytiger::tt_counties(state = 49)
#' matchedcty <- geo_match(from = cnty, to = cd)
#' # use counties as precincts and let the plan be their center match:
#' r2dra(cnty, matchedcty, 'UT', 'r2dra_ex.csv')
#' }
r2dra <- function(precincts, plan, state, path, epsg = 3857) {
  if (missing(precincts)) {
    cli::cli_abort('{.arg precincts} is a required input.')
  }
  if (!'sf' %in% class(precincts)) {
    cli::cli_abort('{.arg precincts} must be an sf dataframe')
  }

  if (missing(plan)) {
    cli::cli_abort('{.arg plan} is a required input.')
  }
  if ('character' %in% class(plan)) {
    plan <- precincts[[plan]]
  }

  if (missing(state)) {
    cli::cli_abort('{.arg state} is a required input.')
  }

  shp <- tinytiger::tt_blocks(state, year = 2020)

  pairs <- make_planar_pair(precincts, shp)
  precincts <- pairs$x
  shp <- pairs$y

  matches <- geo_match(from = shp, to = precincts, epsg = FALSE)

  dist <- plan[matches]

  out <- tibble(GEOID20 = shp$GEOID20, District = dist)

  # return if no path
  if (missing(path)) {
    return(out)
  } else {
    if (stringr::str_sub(path, start = -4) == '.csv') {
      suc <- try(readr::write_csv(x = out, file = path), silent = TRUE)
    } else {
      suc <- try(readr::write_csv(x = out, file = paste0(path, '.csv')), silent = TRUE)
    }
    if (!'data.frame' %in% class(suc)) {
      cli::cli_warn('save to path failed. Returning dataframe to be saved manually.')
    }
  }

  out
}

globalVariables(c('District', 'District_DRA'))
