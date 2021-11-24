#' Get the kind of center of each shape
#'
#' Returns points within the shape, near the center.
#' Uses the centroid if that's in the shape, or point on surface if not.
#'
#' @param shp An sf dataframe
#'
#' @return An sf dataframe where geometry is the center(ish) of each shape in shp
#' @export
#' 
#' @concept leftover
#' 
#' @examples
#' data(towns)
#' st_centerish(towns)
#' 
st_centerish <- function(shp){
  
  cent <- geos::geos_centroid(shp)
  
  if(nrow(shp) > 1){
    
    outside <- !geos::geos_within(cent, shp)
    
    if (any(outside)) {
      pts <- geos::geos_point_on_surface(shp[outside, ])
      cent[outside] <- pts
    }
    
  } else {
    
    outside <- !geos::geos_within(x = cent, y = shp)
    
    if(is.na(outside)){
      outside <- TRUE
    }
    
    if (outside) {
      cent <- geos::geos_point_on_surface(shp)
    }
    
  }

  sf::st_geometry(shp) <- sf::st_as_sfc(cent)
  shp
}
