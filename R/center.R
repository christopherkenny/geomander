#' Get the kind of center of each shape
#'
#' Returns points within the shape, near the center.
#' Uses the centroid if that's in the shape, or point on surface if not.
#'
#' @param shp An sf dataframe
#'
#' @return An sf dataframe where geometry is the center(ish) of each shape in shp
#' @export
#' @importFrom sf st_geometry st_geometry<- st_within st_centroid st_point_on_surface
#' @examples
st_centerish <- function(shp){
  
  suppressWarnings( cent <- st_centroid(shp) )
  
  if(nrow(shp) > 1){

    outside <- sapply(1:nrow(shp), 
                      function(x){suppressMessages(st_within(x = cent[x,], y = shp[x,], sparse = FALSE))})
    suppressWarnings( pts <- st_point_on_surface(shp[outside, ]) )
    suppressWarnings( st_geometry(cent[outside,]) <- st_geometry(pts) )
    
  } else {
    
    
    outside <- as.logical(st_within(x = cent, y = shp, sparse = FALSE))
    if(is.na(outside)){
      outside <- TRUE
    }
    
    suppressWarnings( st_geometry(cent[outside]) <- st_geometry(st_point_on_surface(shp[outside])) )
    
    
  }

  return(cent)
}
