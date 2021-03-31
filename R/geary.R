#' Compute Local Geary's C
#'
#' @param shp sf data frame. Optional if adj or spatial_mat provided.
#' @param adj zero indexed adjacency list. Optional if shp or spatial_mat provided.
#' @param wts Required. Numeric vector with weights to use for Moran's I.
#' @param spatial_mat matrix of spatial weights. Not required if shp or adj provided.
#'
#' @return numeric vector 
#' @export
#' 
#' @examples \dontrun{
#' data("checkerboard")
#' checkerboard <- checkerboard %>% mutate(m = as.numeric((id+i) %% 2 == 0))
#' local_geary(shp = checkerboard, wts = checkerboard$m)
#' }
local_gearys <- function(shp, adj, wts, spatial_mat){
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
  
  
  out <- localgeary(wts, mat)
  
  
  out <- tibble(geary = out)
  return(out)
  
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
#'
#' @return double
#' @export
#' @examples \dontrun{
#' data("checkerboard")
#' checkerboard <- checkerboard %>% mutate(m = as.numeric((id+i) %% 2 == 0))
#' global_geary(shp = checkerboard, wts = checkerboard$m)
#' }
global_geary <- function(shp, adj, wts, spatial_mat){
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
  
  
  out <- globalgeary(wts, mat)
  
  return(out)
  
}