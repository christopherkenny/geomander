#' Filter to Intersecting Pieces
#'
#' @param from Required. sf dataframe. the geography to subset
#' @param to Required. sf dataframe. the geography that from must intersect
#' @param bool Optional, defaults to FALSE. Should this just return a logical vector?
#' @templateVar epsg TRUE
#' @template  template
#'
#' @return sf data frame or logical vector if bool == TRUE
#' @export
#'
#' @concept datatable
#'
#' @examples
#' \dontrun{
#' # Needs Census Bureau API
#' data(towns)
#' block <- create_block_table('NY', 'Rockland')
#' geo_filter(block, towns)
#' }
#'
#' data(towns)
#' data(rockland)
#' sub <- geo_filter(rockland, towns)
#'
geo_filter <- function(from, to, bool = FALSE, epsg = 3857) {
  pairs <- make_planar_pair(from, to, epsg = epsg)
  from <- pairs$x
  to <- pairs$y
  ints <- geos::geos_intersects(
    from,
    geos::geos_unary_union(geos::geos_make_collection(to))
  )

  if (bool) {
    return(ints)
  }

  from[ints, ]
}


#' Trim Away Small Pieces
#'
#' @param from Required. sf dataframe. the geography to subset
#' @param to Required. sf dataframe. the geography that from must intersect
#' @param thresh Percent as decimal of an area to trim away. Default is .01, which is 1%.
#' @param bool Optional, defaults to FALSE. Should this just return a logical vector?
#' @templateVar epsg TRUE
#' @template  template
#'
#' @return sf data frame or logical vector if bool=TRUE
#' @export
#'
#' @concept datatable
#' @examples \dontrun{
#' # Needs Census Bureau API
#' data(towns)
#' block <- create_block_table('NY', 'Rockland')
#' geo_trim(block, towns, thresh = 0.05)
#' }
#'
#' data(towns)
#' data(rockland)
#' sub <- geo_filter(rockland, towns)
#' rem <- geo_trim(sub, towns, thresh = 0.05)
#'
geo_trim <- function(from, to, thresh = 0.01, bool = FALSE, epsg = 3857) {
  pairs <- make_planar_pair(from, to, epsg = epsg)
  from <- pairs$x
  to <- pairs$y

  ints <- geos::geos_intersection(from, geos::geos_unary_union(geos::geos_make_collection(to)))
  area <- geos::geos_area(from)
  areaints <- rep(0, nrow(from))
  areaints <- geos::geos_area(geos::geos_make_valid(ints)) # , NA_on_exception = TRUE in valid
  keep <- as.numeric(areaints / area) > thresh

  keep[is.na(keep)] <- FALSE

  if (bool) {
    return(keep)
  }

  from[keep, ]
}
