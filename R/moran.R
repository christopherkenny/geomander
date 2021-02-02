#' Compute Local Moran's I
#'
#' @param shp sf data frame. Optional if adj or spatial_mat provided.
#' @param adj zero indexed adjacency list. Optional if shp or spatial_mat provided.
#' @param wts Required. Numeric vector with weights to use for Moran's I.
#' @param spatial_mat matrix of spatial weights. Optional if shp or adj provided.
#'
#' @return
#' @export
#'
#' @examples
local_morans <- function(shp, adj, wts, spatial_mat){
  if(missing(shp) & missing(adj) & missing(spatial_mat)){
    stop('Please supply an argument to at least one of shp or adj or spatial_mat.')
  }
  
  if(missing(adj) & missing(spatial_mat)){
    adj <- st_relate(shp, shp, pattern = 'F***1****')
    adj <- lapply(adj, FUN = function(x){x-1L})
  }

  if(missing(spatial_mat)){
    mat <- adjlist2matrix(adj)
  } else {
    if(nrow(mat) != ncol(mat)){
      stop('spatial_mat must be square.')
    }
    
    if(length(wts) != nrow(spatial_mat)){
      stop('wts and spatial_mat have different lengths.')
    }
  }

  
  out <- localmoran(wts, mat)
  
  
  out <- tibble(moran = out$moran, expectation = out$expectation, variance = out$variance)
  return(out)

}

#' Compute Global Moran's I
#'
#' Computes the Global Moran's I statistic and expectation. Can produce spatial weights
#' from an adjacency or sf data frame, in which case the spatial_mat is a contiguity
#' matrix. Users can also provide a spatial_mat argument directly.
#'
#' @param shp sf data frame. Optional if adj or spatial_mat provided.
#' @param adj zero indexed adjacency list. Optional if shp or spatial_mat provided.
#' @param wts Required. Numeric vector with weights to use for Moran's I.
#' @param spatial_mat matrix of spatial weights. Optional if shp or adj provided.
#'
#' @return
#' @export
#'
#' @examples
global_morans <- function(shp, adj, wts, spatial_mat){
  if(missing(shp) & missing(adj) & missing(spatial_mat)){
    stop('Please supply an argument to at least one of shp or adj or spatial_mat.')
  }
  
  if(missing(adj) & missing(spatial_mat)){
    adj <- st_relate(shp, shp, pattern = 'F***1****')
    adj <- lapply(adj, FUN = function(x){x-1L})
  }
  
  if(missing(spatial_mat)){
    mat <- adjlist2matrix(adj)
  } else {
    if(nrow(mat) != ncol(mat)){
      stop('spatial_mat must be square.')
    }
    
    if(length(wts) != nrow(spatial_mat)){
      stop('wts and spatial_mat have different lengths.')
    }
  }
  
  
  out <- globalmoran(wts, mat)
  
  return(out)
  
}