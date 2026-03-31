#' Match Features Across Geographic Layers
#'
#' Match each row of `from` to one row of `to`, typically when `from` is a finer
#' geography nested inside `to`. The result is an integer vector of row indices in
#' `to` and can be reused by downstream helpers such as [estimate_up()],
#' [estimate_down()], and [block2prec()].
#'
#' @param from An `sf` object to match from, usually the smaller geography.
#' @param to An `sf` object to match to, usually the larger geography.
#' @param method Matching method. One of `"center"`, `"centroid"`, `"point"`,
#'   `"circle"`, or `"area"`.
#' @param by Optional character scalar restricting matching within shared groups.
#'   Use a single value when `from` and `to` use the same column name, or a named
#'   scalar such as `c(county_from = "county_to")` when the column names differ.
#' @param tiebreaker Logical. If `TRUE`, ambiguous or unmatched features are
#'   assigned to the nearest feature in `to`. If `FALSE`, unmatched rows receive
#'   `-1` and rows with multiple matches receive `-2`.
#' @templateVar epsg TRUE
#' @template template
#'
#' @details
#'
#' Methods are as follows:
#' - `"centroid"`: match using the geometric centroid of each row in `from`.
#' - `"center"`: use the centroid when it lies inside the polygon and otherwise
#'   fall back to a point-on-surface.
#' - `"point"`: use point on surface for each row in `from`.
#' - `"circle"`: use the centroid of the maximum inscribed circle.
#' - `"area"`: match to the row in `to` with the largest area overlap.
#'
#' When the output does not reference every row of `to`, an attribute
#' `"matching_max"` is attached and records `nrow(to)`. Functions such as
#' [block2prec()] use that attribute to preserve empty target groups.
#'
#' @return integer vector of length `nrow(from)`. Values are row indices in `to`,
#'   or `-1` / `-2` when `tiebreaker = FALSE`. Empty geometries are returned as
#'   `NA`.
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
