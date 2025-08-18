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
#'   sf::st_union(checkerboard |> filter(i < 4)),
#'   sf::st_union(checkerboard |> filter(i >= 4))
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
  
  # setup by ----
  if (!is.null(by)) {
    
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
    
    vals <- unique(from[[col_from]])
    
    col_from_data <- from[[col_from]]
    col_to_data <- to[[col_to]]
    
    from_idx <- lapply(vals, function(v) which(col_from_data == v))
    to_idx <- lapply(vals, function(v) which(col_to_data == v))
    
  }
  
  # start from / to optimizing ----
  pairs <- make_planar_pair(sf::st_geometry(from), sf::st_geometry(to), epsg = epsg)
  from <- pairs$x
  to <- pairs$y
  to_tree <- geos::geos_strtree(to)

  if (is.null(by)) {
    if (method %in% c('center', 'centroid', 'point', 'circle')) {
      if (method == 'center') {
        op <- function(x) geos_centerish(x, epsg = epsg)
      } else if (method == 'circle') {
        op <- function(x) geos_circle_center(x, epsg = epsg)
      } else if (method == 'centroid') {
        op <- geos::geos_centroid
      } else {
        op <- geos::geos_point_on_surface
      }

      pts <- op(from)

      ints <- geos::geos_intersects_matrix(pts, to_tree)
      if (any(lengths(ints) != 1)) {
        idx <- which(lengths(ints) != 1)

        if (tiebreaker) {
          ints[idx] <- geos::geos_nearest_indexed(geom = from[idx, ], tree = to_tree)
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
      ints <- largest_intersection_geos(
        x = geos::geos_make_valid(from),
        y = geos::geos_make_valid(to)
      )

      if (any(is.na(ints))) {
        idx <- which(is.na(ints))

        if (tiebreaker) {
          ints[idx] <- geos::geos_nearest_indexed(geom = from[idx, ], tree = to_tree)
        } else {
          ints[idx] <- -1L
        }
      }
    }

    ints[geos::geos_is_empty(from)] <- NA_real_
  } else {
    # create corresponding subset lists
    from_l <- lapply(from_idx, function(v) {
      from[v]
    })
    to_l <- lapply(to_idx, function(v) {
      to[v]
    })

    # id entries
    int_l <- lapply(
      seq_along(vals),
      function(i) {
        if (length(from_l[[i]]) == 0 || length(to_l[[i]]) == 0) {
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
    ints <- rep.int(NA_integer_, times = length(from))

    for (i in seq_along(vals)) {
      from_idx <- which(col_from_data == vals[i])
      to_idx <- which(col_to_data == vals[i])
      ints[from_idx] <- to_idx[int_l[[i]]]
    }
  }

  ints <- as.integer(ints)
  if (is.na(max(ints))) {
    cat(ints)
  }
  if (max(ints) != length(to)) {
    attr(ints, 'matching_max') <- length(to)
  }
  ints
}
