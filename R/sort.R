#' Sort Precincts
#'
#' Reorders precincts by distance from the NW corner of the bounding box.
#'
#' @param shp sf dataframe, required.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return sf dataframe
#' @export
#'
#' @concept fix
#'
#' @examples
#' data(checkerboard)
#' geo_sort(checkerboard)
geo_sort <- function(shp, epsg = 3857) {
  if (missing(shp)) {
    cli::cli_abort('{.arg shp} is required.')
  }

  shp <- make_planar_pair(shp, epsg = epsg)$x

  bbox <- bbox_geos(shp)
  pt <- geos::geos_make_point(x = geos::geos_xmin(bbox), y = geos::geos_ymax(bbox))
  cent <- geos::geos_centroid(shp)
  dists <- geos::geos_distance(pt, cent)
  idx <- sort(dists, index.return = TRUE)
  shp %>% dplyr::slice(idx$ix)
}
