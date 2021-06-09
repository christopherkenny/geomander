#' Sort Precincts
#' 
#' Reorders precincts by distance from the NW corner of the bounding box.
#'
#' @param shp sf dataframe, required.
#'
#' @return sf dataframe
#' @export
#' @examples \dontrun{
#' data(va18sub)
#' geo_sort(va18sub)
#' }
geo_sort <- function(shp){
  if(missing(shp)){
    stop('shp is required.')
  }
  
  bbox <- sf::st_bbox(shp)
  pt <- sf::st_point(x = c(bbox$xmin, bbox$ymax))
  suppressWarnings(cent <- sf::st_centroid(shp))
  dists <- sf::st_distance(pt, cent)
  idx <- sort(dists, index.return = TRUE)
  return(shp %>% dplyr::slice(idx$ix))
}