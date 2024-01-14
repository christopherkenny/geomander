#' Match Across Geographic Layers
#'
#' @param from smaller geographic level to match up from
#' @param to larger geographic level to be matched to
#' @param method string from 'center', 'centroid', 'point', 'circle', or 'area' for matching method
#' @param by A character vector to match by. One element if both `from` and `to` share the subsetting column name.
#' One element with a name (for `from`) and one element (for `to`).
#' @param tiebreaker Should ties be broken? boolean. If FALSE, precincts with no
#' matches get value -1 and precincts with multiple matches get value -2.
#' @templateVar epsg TRUE
#' @template template
#'
#' @details
#'
#' Methods are as follows:
#' - centroid: matches each element of `from` to the `to` entry that the geographic centroid intersects
#' - center: very similar to centroid, but it matches an arbitrary center point within `from`
#'   if the centroid of `from` is outside the bounds of from. (This happens for non-convex shapes only).
#' - point: matches each element of `from` to the `to` entry that the "point on surface" intersects.
#' - circle: matches each element of `from` to the `to` entry that the centroid
#'   of the maximum inscribed circle intersects
#' - area: matches each element of `from` to the `to` element which has the largest area overlap
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
geo_match <- function(from, to, method = 'center', by = NULL, tiebreaker = TRUE, epsg = 3857) {
  match.arg(arg = method, choices = c('center', 'centroid', 'point', 'circle', 'area'))

  if (missing(from)) {
    cli::cli_abort('Please provide an argument to {.arg from}.')
  }
  if (missing(to)) {
    cli::cli_abort('Please provide an argument to {.arg to}.')
  }

  pairs <- make_planar_pair(from, to, epsg = epsg)
  from <- pairs$x
  to <- pairs$y

  if (is.null(by)) {
    if (method %in% c('center', 'centroid', 'point', 'circle')) {
      if (method == 'center') {
        op <- function(x) st_centerish(x, epsg = epsg)
      } else if (method == 'circle') {
        op <- function(x) st_circle_center(x, epsg = epsg)
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
          for (i in seq_along(ints)) {
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
      to <- to %>% dplyr::mutate(toid = dplyr::row_number())
      from <- from %>% dplyr::mutate(fromid = dplyr::row_number())
      ints <- largest_intersection_geos(
        x = geos::geos_make_valid(from),
        y = geos::geos_make_valid(to)
      )

      if (any(is.na(ints))) {
        idx <- which(is.na(ints))

        if (tiebreaker) {
          for (i in seq_along(idx)) {
            nnb <- nn_geos(x = from[idx[i], ], y = to)
            ints[idx[i]] <- nnb
          }
        } else {
          for (i in seq_along(idx)) {
            ints[idx[i]] <- -1L
          }
        }
      }
    }

    ints[geos::geos_is_empty(from)] <- NA_real_
  } else {
    # check by ----
    if (length(by) != 1) {
      cli::cli_abort('{.arg by} must be {.val NULL} or length 1.')
    }

    if (length(names(by)) > 0) {
      col_from <- names(by)
      col_to <- unname(by)
    } else {
      col_to <- col_from <- by
    }

    if (!col_from %in% names(from)) {
      cli::cli_abort('{.arg by} entry for from {col_from} not found in {.arg from}.')
    }
    if (!col_to %in% names(to)) {
      cli::cli_abort('{.arg to} entry for from {col_to} not found in {.arg to}.')
    }

    vals <- unique(from[[col_from]]) # vals_from
    # vals_to <- unique(to[col_to])

    # create corresponding subset lists
    from_l <- lapply(vals, function(v) {
      from %>%
        dplyr::filter(.data[[col_from]] == v)
    })
    to_l <- lapply(vals, function(v) {
      to %>%
        dplyr::filter(.data[[col_to]] == v)
    })

    # id entries
    int_l <- lapply(
      seq_along(vals),
      function(i) {
        if (nrow(from_l[[i]]) == 0 || nrow(to_l[[i]]) == 0) {
          return(integer(0)) # perhaps cli::cli_abort('something about values being in both')
        }
        geo_match(
          from = from_l[[i]], to = to_l[[i]],
          method = method, tiebreaker = tiebreaker,
          epsg = FALSE
        )
      }
    )

    # build index
    ints <- rep.int(NA_integer_, times = nrow(from))

    for (i in seq_along(vals)) {
      from_idx <- which(from[[col_from]] == vals[i])
      to_idx <- which(to[[col_to]] == vals[i])
      ints[from_idx] <- to_idx[int_l[[i]]]
    }
  }

  as.integer(ints)
}

globalVariables(c('fromid', 'toid', 'area'))
