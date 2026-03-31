#' Compute Standardized Getis-Ord G*i
#'
#' Returns standardized local Getis-Ord G*i scores using either an `sf` object, a
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
#' @concept spatcorr
#'
#' @return numeric vector of standardized G*i scores, one per observation
#' @export
#' @examples
#' library(dplyr)
#' data('checkerboard')
#' checkerboard <- checkerboard |> mutate(m = as.numeric((id + i) %% 2 == 0))
#' gstar_i(shp = checkerboard, wts = checkerboard$m)
gstar_i <- function(shp, adj, wts, spatial_mat, epsg = 3857) {
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
      cli::cli_abort('spatial_mat must be square.')
    }

    if (length(wts) != nrow(spatial_mat)) {
      cli::cli_abort('wts and spatial_mat have different lengths.')
    }
  }

  localgstar(wts, mat)
}
