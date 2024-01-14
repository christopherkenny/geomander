#' Split a Precinct
#'
#' States often split a precinct when they create districts but rarely provide the
#' geography for the split precinct. This allows you to split a precinct using a
#' lower geography, typically blocks.
#'
#' @param lower The lower geography that makes up the precinct, this is often a
#' block level geography.
#' @param precinct The single precinct that you would like to split.
#' @param split_by The upper geography that you want to split precinct by
#' @param lower_wt Optional. Numeric weights to give to each precinct, typically
#' VAP or population.
#' @param split_by_id Optional. A string that names a column in split_by that
#' identifies each observation in split_by
#' @templateVar epsg TRUE
#' @template template
#'
#' @return sf data frame with precinct split
#' @export
#'
#' @concept fix
#'
#' @examples
#' library(sf)
#' data(checkerboard)
#' low <- checkerboard %>% dplyr::slice(1:3, 9:11)
#' prec <- checkerboard %>%
#'   dplyr::slice(1:3) %>%
#'   dplyr::summarize(geometry = sf::st_union(geometry))
#' dists <- checkerboard %>%
#'   dplyr::slice(1:3, 9:11) %>%
#'   dplyr::mutate(dist = c(1, 2, 2, 1, 3, 3)) %>%
#'   dplyr::group_by(dist) %>%
#'   dplyr::summarize(geometry = sf::st_union(geometry))
#'
#' split_precinct(low, prec, dists, split_by_id = 'dist')
split_precinct <- function(lower, precinct, split_by, lower_wt, split_by_id, epsg = 3857) {
  if (!missing(lower_wt)) {
    if (length(lower_wt) != nrow(lower)) {
      cli::cli_abort('{.arg lower_wt} must have the same number of entries as {.arg lower}')
    }

    if (!is.numeric(lower_wt)) {
      cli::cli_abort('{.arg lower_wt} must be a numeric vector to allow for weighting.')
    }
  }

  if (nrow(precinct) != 1) {
    cli::cli_abort('Please provide only one geography to {.arg precinct}.')
  }

  if (nrow(split_by) < 2) {
    cli::cli_abort('{.arg split_by} requires at least two geographies to consider.')
  }

  pairs <- make_planar_pair(lower, precinct, epsg = epsg)
  lower <- pairs$x
  precinct <- pairs$y
  pairs <- make_planar_pair(lower, split_by, epsg = epsg)
  split_by <- pairs$y

  lower <- lower %>%
    geo_filter(precinct, epsg = FALSE) %>%
    geo_trim(precinct, epsg = FALSE)

  split_by <- split_by %>%
    geo_filter(precinct, epsg = FALSE)

  matches <- geo_match(from = lower, to = split_by, epsg = FALSE)

  out_geo <- lower %>%
    dplyr::select("geometry") %>%
    dplyr::mutate(new = matches) %>%
    dplyr::group_by(.data$new) %>%
    dplyr::summarize(geometry = sf::st_union(.data$geometry))


  if (!missing(lower_wt)) {
    out_wt <- tibble(new = matches, wt = lower_wt) %>%
      dplyr::group_by(.data$new) %>%
      dplyr::summarize(wt = sum(.data$lower_wt, na.rm = TRUE), .groups = 'drop')

    out_geo <- dplyr::left_join(out_geo, out_wt, by = c('new'))
  }

  if (!missing(split_by_id)) {
    if (split_by_id %in% names(split_by)) {
      out_geo$id <- split_by[[split_by_id]][out_geo$new]
    } else {
      cli::cli_warn('{.arg split_by_id} provided, but no column with that name found.')
    }
  }

  out_geo
}
