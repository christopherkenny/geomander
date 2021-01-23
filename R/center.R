#' Get the kind of center of each shape
#'
#' Returns points within the shape, near the center.
#' Uses the centroid if that's in the shape, or point on surface if not.
#'
#' @param shp An sf shapefile
#'
#' @return
#' @export
#'
#' @examples
st_centerish <- function(shp){
  cent <- st_centroid(shp)
  
  
  if(nrow(shp) > 1){
    outside <- diag(st_within(x = cent, y = shp, spare = FALSE))
    pts <- st_point_on_surface(shp[outside, ])
    st_geometry(cent[outside,]) <- st_geometry(pts)
  } else {
    
    
    outside <- as.logical(st_within(x = cent, y = shp, spare = FALSE))
    if(is.na(outside)){
      outside <- TRUE
    }
    
    st_geometry(cent[outside]) <- st_geometry(st_point_on_surface(shp[outside]))
    
    
  }


  
  return(cent)
}
