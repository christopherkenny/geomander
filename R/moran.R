#' Compute Local Moran's I
#'
#' Compute Local Moran's I for each observation using either an `sf` object, a
#' zero-indexed adjacency list, or a spatial weights matrix.
#'
#' @param shp `sf` dataframe. Optional if `adj` or `spatial_mat` is supplied.
#' @param adj Zero-indexed adjacency list. Optional if `shp` or `spatial_mat` is
#'   supplied.
#' @param wts Numeric vector of observed values.
#' @param spatial_mat Square spatial weights matrix. Optional if `shp` or `adj`
#'   is supplied.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return tibble with columns `moran`, `expectation`, and `variance`
#' @export
#'
#' @concept spatcorr
#'
#' @examples
#' library(dplyr)
#' data('checkerboard')
#' checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) %% 2 == 0))
#' local_morans(shp = checkerboard, wts = checkerboard$m)
local_morans <- function(shp, adj, wts, spatial_mat, epsg = 3857) {
  if (missing(shp) & missing(adj) & missing(spatial_mat)) {
    cli::cli_abort('Please supply an argument to at least one of {.arg shp} or {.arg adj} or {.arg spatial_mat}.')
  }

  if (missing(adj) & missing(spatial_mat)) {
    adj <- adjacency(shp, epsg = epsg)
  }

  if (missing(spatial_mat)) {
    mat <- adjlist2matrix(adj)
  } else {
    if (nrow(mat) != ncol(mat)) {
      cli::cli_abort('{.arg spatial_mat} must be square.')
    }

    if (length(wts) != nrow(spatial_mat)) {
      cli::cli_abort('{.arg wts} and {.arg spatial_mat} have different lengths.')
    }
  }


  out <- localmoran(wts, mat)
  tibble(moran = out$moran, expectation = out$expectation, variance = out$variance)
}

#' Compute Global Moran's I
#'
#' Computes the Global Moran's I statistic and expectation. Can produce spatial weights
#' from an adjacency or sf dataframe, in which case the spatial_mat is a contiguity
#' matrix. Users can also provide a spatial_mat argument directly.
#'
#' @param shp `sf` dataframe. Optional if `adj` or `spatial_mat` is supplied.
#' @param adj Zero-indexed adjacency list. Optional if `shp` or `spatial_mat` is
#'   supplied.
#' @param wts Numeric vector of observed values.
#' @param spatial_mat Square spatial weights matrix. Optional if `shp` or `adj`
#'   is supplied.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return named list returned by the underlying C++ implementation, including the
#'   global statistic and its moments.
#' @export
#'
#' @concept spatcorr
#'
#' @examples
#' library(dplyr)
#' data('checkerboard')
#' checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) %% 2 == 0))
#' global_morans(shp = checkerboard, wts = checkerboard$m)
global_morans <- function(shp, adj, wts, spatial_mat, epsg = 3857) {
  if (missing(shp) & missing(adj) & missing(spatial_mat)) {
    cli::cli_abort('Please supply an argument to at least one of {.arg shp} or {.arg adj} or {.arg spatial_mat}.')
  }

  if (missing(adj) & missing(spatial_mat)) {
    adj <- adjacency(shp, epsg = epsg)
  }

  if (missing(spatial_mat)) {
    mat <- adjlist2matrix(adj)
  } else {
    if (nrow(mat) != ncol(mat)) {
      cli::cli_abort('{.arg spatial_mat} must be square.')
    }

    if (length(wts) != nrow(spatial_mat)) {
      cli::cli_abort('{.arg wts} and {.arg spatial_mat} have different lengths.')
    }
  }

  globalmoran(wts, mat)
}
