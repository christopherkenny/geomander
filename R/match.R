#' Match Across Geographic Layers
#'
#' @param from smaller geographic level to match up from
#' @param to larger geographic level to be matched to
#' @param method string from center, centroid, point, or area for matching method
#' @param tiebreaker Should ties be broken? boolean. If FALSE, precincts with no
#' matches get value -1 and precincts with multiple matches get value -2.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return Integer Vector of matches length(to) with values in 1:nrow(from)
#' @export
#'
#' @concept estimate
#'
#' @examples
#' library(dplyr)
#' data(checkerboard)
#' counties <- sf::st_as_sf(as.data.frame(rbind(
#'   sf::st_union(checkerboard %>% filter(i < 4)),
#'   sf::st_union(checkerboard %>% filter(i >= 4))
#' )))
#'
#' geo_match(from = checkerboard, to = counties)
#' geo_match(from = checkerboard, to = counties, method = 'area')
geo_match <- function(from, to, method = 'center', tiebreaker = TRUE, epsg = 3857) {
  match.arg(arg = method, choices = c('center', 'centroid', 'point', 'area'))

  if (missing(from)) {
    cli::cli_abort('Please provide an argument to {.arg from}.')
  }
  if (missing(to)) {
    cli::cli_abort('Please provide an argument to {.arg to}.')
  }
  
  pairs <- make_planar_pair(from, to, epsg = epsg)
  from <- pairs$x
  to <- pairs$y

  if (method %in% c('center', 'centroid', 'point')) {
    if (method == 'center') {
      op <- function(x) st_centerish(x, epsg = epsg)
    } else if (method == 'centroid') {
      op <- geos::geos_centroid
    } else {
      op <- geos::geos_point_on_surface
    }

    pts <- op(from)

    ints <- geos::geos_intersects_matrix(pts, to)
    if (any(lengths(ints) != 1)) {
      idx <- which(lengths(ints) != 1)

      if (tiebreaker) {
        for (i in seq_along(idx)) {
          nnb <- nn_geos(x = from[idx[i], ], y = to)
          ints[[idx[i]]] <- nnb
        }
      } else {
        for (i in 1:length(ints)) {
          if (length(ints[[i]]) == 0) {
            ints[[i]] <- -1L
          }
          if (length(ints[[i]]) > 1) {
            ints[[i]] <- -2L
          }
        }
      }
    }
  } else {
    to <- to %>% mutate(toid = row_number())
    from <- from %>% mutate(fromid = row_number())
    ints <- largest_intersection_geos(x = geos::geos_make_valid(from), 
                                      y = geos::geos_make_valid(to))

    if (any(is.na(ints))) {
      idx <- which(is.na(ints))

      if (tiebreaker) {
        for (i in 1:length(idx)) {
          nnb <- nn_geos(x = from[idx[i], ], y = to)
          ints[idx[i]] <- nnb
        }
      }
    }
  }

  ints[geos::geos_is_empty(from)] <- NA_real_
  as.integer(ints)
}

globalVariables(c('fromid', 'toid', 'area'))
