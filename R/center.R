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
  
  outside <- diag(st_within(x = cent, y = shp, spare = FALSE))
  
  pts <- st_point_on_surface(shp[outside, ])
  
  st_geometry(cent[outside,]) <- st_geometry(pts)
  
  return(cent)
}
