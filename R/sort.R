#' Sort Precincts
#'
#' Reorders precincts by distance from the NW corner of the bounding box.
#'
#' @param shp sf dataframe, required.
#'
#' @return sf dataframe
#' @export
#'
#' @concept fix
#'
#' @examples
#' data(checkerboard)
#' geo_sort(checkerboard)
geo_sort <- function(shp) {
  
  if (missing(shp)) {
    stop('shp is required.')
  }

  bbox <- bbox_geos(shp)
  pt <- geos::geos_make_point(x = geos::geos_xmin(bbox), y = geos::geos_ymax(bbox))
  cent <- geos::geos_centroid(shp)
  dists <- geos::geos_distance(pt, cent)
  idx <- sort(dists, index.return = TRUE)
  shp %>% dplyr::slice(idx$ix)
}
