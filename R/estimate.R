#' Estimate Down Geography Levels
#'
#' Simple method for estimating data down to a lower level. This is most often useful
#' for getting election data down from a precinct level to a block level in the case
#' that a state or other jurisdiction split precincts when creating districts. Geographic
#' partner to estimate_down.
#'
#' @param from Larger geography level
#' @param to smaller geography level
#' @param wts numeric vector of length nrow(to). Defaults to 1. Typically population or VAP, as a weight to give each precinct.
#' @param value numeric vector of length nrow(from). Defaults to 1. Typically electoral outcomes, as a value to estimate down into blocks.
#' @param method string from center, centroid, point, or area for matching levels
#' @templateVar epsg TRUE
#' @template  template
#'
#' @return numeric vector with each value split by weight
#' @concept estimate
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' set.seed(1)
#' data(checkerboard)
#' counties <- checkerboard %>%
#'   group_by(id <= 32) %>%
#'   summarize(geometry = sf::st_union(geometry)) %>%
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

  tb <- tibble(wts = wts, group = group) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(GTot = sum(wts)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::if_else(GTot == 0, 1, wts)) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(GTot = sum(wts)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cont = wts / GTot)

  tb2 <- tibble(group = 1:length(value), value = value)

  tb <- tb %>%
    dplyr::left_join(tb2, by = 'group') %>%
    dplyr::mutate(out = cont * value)

  tb <- tb %>%
    dplyr::mutate(out = ifelse(is.na(out), 0, out))

  tb$out
}


#' Estimate Down Levels
#'
#' Non-geographic partner function to geo_estimate_down. Allows users to estimate
#' down without the costly matching operation if they've already matched.
#'
#' @param wts numeric vector. Defaults to 1. Typically population or VAP, as a weight to give each precinct.
#' @param value numeric vector. Defaults to 1. Typically electoral outcomes, as a value to estimate down into blocks.
#' @param group matches of length(wts) that correspond to row indices of value. Often, this input is the output of geo_match.
#'
#' @return numeric vector with each value split by weight
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
#' counties <- checkerboard %>%
#'   group_by(id <= 32) %>%
#'   summarize(geometry = sf::st_union(geometry)) %>%
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

  tb <- tibble(wts = wts, group = group) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(GTot = sum(wts)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(wts = dplyr::if_else(GTot == 0, 1, wts)) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(GTot = sum(wts)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cont = wts / GTot)

  tb2 <- tibble(group = 1:length(value), value = value)

  tb <- tb %>%
    dplyr::left_join(tb2, by = 'group') %>%
    dplyr::mutate(out = cont * value)

  tb <- tb %>%
    dplyr::mutate(out = ifelse(is.na(out), 0, out))

  tb$out
}


#' Estimate Up Geography Levels
#'
#' Simple method for aggregating data up to a higher level This is most often useful
#' for getting population data from a block level up to a precinct level.
#' Geographic partner to estimate_up.
#'
#' @param from smaller geography level
#' @param to larger geography level
#' @param value numeric vector of length nrow(from). Defaults to 1.
#' @param method string from center, centroid, point, or area for matching levels
#' @templateVar epsg TRUE
#' @template  template
#'
#' @return numeric vector with each value aggregated by group
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
#' counties <- checkerboard %>%
#'   group_by(id <= 32) %>%
#'   summarize(geometry = sf::st_union(geometry)) %>%
#'   mutate(pop = c(100, 200))
#' geo_estimate_up(from = checkerboard, to = counties, value = checkerboard$i)
geo_estimate_up <- function(from, to, value, method = 'center', epsg = 3857) {
  group <- geo_match(from = from, to = to, method = method, epsg = epsg)

  if (missing(value)) {
    value <- 1
  }
  tb <- tibble(value = value, group = group) %>%
    group_by(group) %>%
    summarize(value = sum(value)) %>%
    arrange(group)

  if (nrow(tb) < nrow(to)) {
    for (i in 1:nrow(to)) {
      if (tb$group[i] != i) {
        tb <- tb %>% add_row(group = i, value = 0, .after = (i - 1))
      }
    }
  }

  tb$value
}

#' Estimate Up Levels
#'
#' Non-geographic partner function to geo_estimate_up. Allows users to aggregate
#' up without the costly matching operation if they've already matched.
#'
#' @param value numeric vector. Defaults to 1. Typically population values.
#' @param group matches of length(value) that correspond to row indices of value.
#' Often, this input is the output of geo_match.
#'
#' @return numeric vector with each value aggregated by group
#'
#' @importFrom tibble tibble add_row
#' @importFrom dplyr group_by ungroup slice pull left_join
#' @export
#' @concept estimate
#' @examples
#' library(dplyr)
#' set.seed(1)
#' data(checkerboard)
#' counties <- checkerboard %>%
#'   group_by(id <= 32) %>%
#'   summarize(geometry = sf::st_union(geometry)) %>%
#'   mutate(pop = c(100, 200))
#' matches <- geo_match(checkerboard, counties)
#' estimate_up(value = checkerboard$i, group = matches)
estimate_up <- function(value, group) {
  if (missing(value)) {
    value <- 1
  }

  tb <- tibble(value = value, group = group) %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(value = sum(value)) %>%
    dplyr::arrange(group)

  if (nrow(tb) < max(group)) {
    for (i in 1:max(group)) {
      if (tb$group[i] != i) {
        tb <- tb %>%
          dplyr::add_row(group = i, value = 0, .after = (i - 1))
      }
    }
  }

  tb$value
}


globalVariables(c('GTot', 'cont', 'out', 'group', 'value'))
