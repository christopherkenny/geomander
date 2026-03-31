#' Filter to Intersecting Pieces
#'
#' Keep only the rows of `from` whose geometry intersects any geometry in `to`.
#'
#' @param from `sf` object to subset.
#' @param to `sf` object used as the target geography.
#' @param bool Logical. If `TRUE`, return only the logical keep vector.
#' @templateVar epsg TRUE
#' @template  template
#'
#' @return subset of `from`, or a logical vector when `bool = TRUE`
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
#' Keep only rows of `from` whose intersection with `to` covers more than a given
#' share of the row's area.
#'
#' @param from `sf` object to subset.
#' @param to `sf` object used as the target geography.
#' @param thresh Minimum retained overlap as a proportion of each row's area.
#' @param bool Logical. If `TRUE`, return only the logical keep vector.
#' @templateVar epsg TRUE
#' @template  template
#'
#' @return subset of `from`, or a logical vector when `bool = TRUE`
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
