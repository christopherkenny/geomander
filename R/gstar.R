#' Compute Standardized Getis Ord G*i
#'
#' Returns the Getis Ord G*i in standardized form.
#'
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
#' @return vector of G*i scores
#' @export
#' @examples
#' library(dplyr)
#' data('checkerboard')
#' checkerboard <- checkerboard %>% mutate(m = as.numeric((id + i) %% 2 == 0))
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
