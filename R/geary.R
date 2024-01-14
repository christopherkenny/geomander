#' Compute Local Geary's C
#'
#' @param shp sf data frame. Optional if adj or spatial_mat provided.
#' @param adj zero indexed adjacency list. Optional if shp or spatial_mat provided.
#' @param wts Required. Numeric vector with weights to use for Moran's I.
#' @param spatial_mat matrix of spatial weights. Not required if shp or adj provided.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return numeric vector
#' @export
#'
#' @concept spatcorr
#'
#' @examples
#' library(dplyr)
#' data('checkerboard')
#' checkerboard <- checkerboard %>% mutate(m = as.numeric((id + i) %% 2 == 0))
#' local_gearys(shp = checkerboard, wts = checkerboard$m)
local_gearys <- function(shp, adj, wts, spatial_mat, epsg = 3857) {
  if (missing(shp) & missing(adj) & missing(spatial_mat)) {
    cli::cli_abort('Please supply an argument to at least one of shp or adj or spatial_mat.')
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

  tibble(geary = localgeary(wts, mat))
}


#' Compute Global Geary's C
#'
#' Computes the Global Geary's Contiguity statistic. Can produce spatial weights
#' from an adjacency or sf data frame, in which case the spatial_mat is a contiguity
#' matrix. Users can also provide a spatial_mat argument directly.
#'
#' @param shp sf data frame. Optional if adj or spatial_mat provided.
#' @param adj zero indexed adjacency list. Optional if shp or spatial_mat provided.
#' @param wts Required. Numeric vector with weights to use for Moran's I.
#' @param spatial_mat matrix of spatial weights. Optional if shp or adj provided.
#' @templateVar epsg TRUE
#' @template template
#'
#' @concept spatcorr
#'
#' @return double
#' @export
#' @examples
#' library(dplyr)
#' data('checkerboard')
#' checkerboard <- checkerboard %>% mutate(m = as.numeric((id + i) %% 2 == 0))
#' global_gearys(shp = checkerboard, wts = checkerboard$m)
global_gearys <- function(shp, adj, wts, spatial_mat, epsg = 3857) {
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


  out <- globalgeary(wts, mat)

  out
}
