#' Estimate Values Down to a Finer Geography
#'
#' Split values observed on a larger geography across a smaller geography after
#' first matching each row of `to` to a row of `from`. This is commonly used to
#' distribute precinct-level election totals to blocks.
#'
#' @param from Larger geography level containing the observed values.
#' @param to Smaller geography level to estimate onto.
#' @param wts Numeric vector of length `nrow(to)`. Used to allocate each matched
#'   value across rows of `to`. Defaults to `1`, which splits evenly within each
#'   matched group.
#' @param value Numeric vector of length `nrow(from)` containing the values to
#'   split downward. Defaults to `1`.
#' @param method Matching method passed to [geo_match()].
#' @templateVar epsg TRUE
#' @template  template
#'
#' @details
#' If all weights for a matched group are zero, the function falls back to equal
#' allocation within that group. Rows in `to` that do not match any row of
#' `from` receive `0`.
#'
#' @return numeric vector of length `nrow(to)` with estimated values
#' @concept estimate
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(1)
#' data(checkerboard)
#' counties <- checkerboard |>
#'   group_by(id <= 32) |>
#'   summarize(geometry = sf::st_union(geometry)) |>
#'   mutate(pop = c(100, 200))
#' geo_estimate_down(from = counties, to = checkerboard, value = counties$pop)
geo_estimate_down <- function(from, to, wts, value, method = 'center', epsg = 3857) {
  group <- geo_match(from = to, to = from, method = method, epsg = epsg)

  if (missing(wts)) {
    wts <- 1
  }
  if (missing(value)) {
    value <- rep(1, nrow(from))
  }

  tb <- tibble(wts = wts, group = group) |>
    dplyr::group_by(group) |>
    dplyr::mutate(GTot = sum(wts)) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::if_else(GTot == 0, 1, wts)) |>
    dplyr::group_by(group) |>
    dplyr::mutate(GTot = sum(wts)) |>
    dplyr::ungroup() |>
    dplyr::mutate(cont = wts / GTot)

  tb2 <- tibble(group = 1:length(value), value = value)

  tb <- tb |>
    dplyr::left_join(tb2, by = 'group') |>
    dplyr::mutate(out = cont * value)

  tb <- tb |>
    dplyr::mutate(out = ifelse(is.na(out), 0, out))

  tb$out
}


#' Estimate Values Down Using Precomputed Matches
#'
#' Non-geographic companion to [geo_estimate_down()]. Use this when you already
#' have a vector of matches and want to avoid recomputing them.
#'
#' @param wts Numeric vector of weights. Defaults to `1`.
#' @param value Numeric vector of values on the larger geography. Defaults to `1`.
#' @param group Integer vector of matches of length `length(wts)`. Each entry
#'   should give the row of `value` that the corresponding lower-level unit maps
#'   to, often from [geo_match()].
#'
#' @return numeric vector of length `length(group)` with values split by weight
#'
#' @importFrom tibble tibble
#' @importFrom dplyr group_by ungroup slice pull left_join
#'
#' @concept estimate
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(1)
#' data(checkerboard)
#' counties <- checkerboard |>
#'   group_by(id <= 32) |>
#'   summarize(geometry = sf::st_union(geometry)) |>
#'   mutate(pop = c(100, 200))
#' matches <- geo_match(checkerboard, counties)
#' estimate_down(wts = rep(1, nrow(checkerboard)), value = counties$pop, group = matches)
estimate_down <- function(wts, value, group) {
  if (missing(wts)) {
    wts <- 1
  }
  if (missing(value)) {
    value <- 1
  }

  tb <- tibble(wts = wts, group = group) |>
    dplyr::group_by(group) |>
    dplyr::mutate(GTot = sum(wts)) |>
    dplyr::ungroup() |>
    dplyr::mutate(wts = dplyr::if_else(GTot == 0, 1, wts)) |>
    dplyr::group_by(group) |>
    dplyr::mutate(GTot = sum(wts)) |>
    dplyr::ungroup() |>
    dplyr::mutate(cont = wts / GTot)

  tb2 <- tibble(group = 1:length(value), value = value)

  tb <- tb |>
    dplyr::left_join(tb2, by = 'group') |>
    dplyr::mutate(out = cont * value)

  tb <- tb |>
    dplyr::mutate(out = ifelse(is.na(out), 0, out))

  tb$out
}


#' Aggregate Values Up to a Larger Geography
#'
#' Aggregate values from a smaller geography to a larger geography after matching
#' each row of `from` to a row of `to`. This is commonly used to roll block-level
#' counts up to precincts or districts.
#'
#' @param from Smaller geography level.
#' @param to Larger geography level.
#' @param value Numeric vector of length `nrow(from)` to aggregate. Defaults to
#'   `1`.
#' @param method Matching method passed to [geo_match()].
#' @templateVar epsg TRUE
#' @template  template
#'
#' @details
#' Groups in `to` with no matched rows are included in the output and receive `0`.
#'
#' @return numeric vector of length `nrow(to)` with values aggregated by group
#'
#' @importFrom tibble tibble add_row
#' @importFrom dplyr group_by ungroup slice pull left_join
#'
#' @concept estimate
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(1)
#' data(checkerboard)
#' counties <- checkerboard |>
#'   group_by(id <= 32) |>
#'   summarize(geometry = sf::st_union(geometry)) |>
#'   mutate(pop = c(100, 200))
#' geo_estimate_up(from = checkerboard, to = counties, value = checkerboard$i)
geo_estimate_up <- function(from, to, value, method = 'center', epsg = 3857) {
  group <- geo_match(from = from, to = to, method = method, epsg = epsg)

  if (missing(value)) {
    value <- 1
  }
  tb <- tibble(value = value, group = group) |>
    group_by(group) |>
    summarize(value = sum(value)) |>
    arrange(group)

  if (nrow(tb) < nrow(to)) {
    for (i in 1:nrow(to)) {
      if (tb$group[i] != i) {
        tb <- tb |> add_row(group = i, value = 0, .after = (i - 1))
      }
    }
  }

  tb$value
}

#' Aggregate Values Up Using Precomputed Matches
#'
#' Non-geographic companion to [geo_estimate_up()]. Use this when you already
#' have a vector of group assignments.
#'
#' @param value Numeric vector to aggregate. Defaults to `1`.
#' @param group Integer vector of length `length(value)` giving the destination
#'   row for each value, often from [geo_match()].
#'
#' @return numeric vector of length `max(group)` with values aggregated by group
#'
#' @importFrom tibble tibble add_row
#' @importFrom dplyr group_by ungroup slice pull left_join
#' @export
#' @concept estimate
#' @examples
#' library(dplyr)
#' set.seed(1)
#' data(checkerboard)
#' counties <- checkerboard |>
#'   group_by(id <= 32) |>
#'   summarize(geometry = sf::st_union(geometry)) |>
#'   mutate(pop = c(100, 200))
#' matches <- geo_match(checkerboard, counties)
#' estimate_up(value = checkerboard$i, group = matches)
estimate_up <- function(value, group) {
  if (missing(value)) {
    value <- 1
  }

  tb <- tibble(value = value, group = group) |>
    dplyr::group_by(group) |>
    dplyr::summarize(value = sum(value)) |>
    dplyr::arrange(group)

  if (nrow(tb) < max(group)) {
    for (i in 1:max(group)) {
      if (tb$group[i] != i) {
        tb <- tb |>
          dplyr::add_row(group = i, value = 0, .after = (i - 1))
      }
    }
  }

  tb$value
}


globalVariables(c('GTot', 'cont', 'out', 'group', 'value'))
