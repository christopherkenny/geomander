
#' Filter to Intersecting Pieces
#'
#' @param from Required. sf dataframe. the geography to subset
#' @param to Required. sf dataframe. the geography that from must intersect
#' @param bool Optional, defaults to FALSE. Should this just return a logical vector?
#'
#' @return sf data frame or logical vector if bool=TRUE
#' @export
#'
#' @importFrom sf st_intersects
#' @examples
geo_filter <- function(from, to, bool = FALSE){
  
  ints <- sf::st_intersects(x = from, y = to, sparse = FALSE)
  
  if(bool){
    return(ints)
  }
  
  from <- from[ints,]

  return(from)
}


#' Trim Away Small Pieces
#'
#' @param from Required. sf dataframe. the geography to subset
#' @param to Required. sf dataframe. the geography that from must intersect
#' @param thresh Percent as decimal of an area to trim away.
#' @param bool Optional, defaults to FALSE. Should this just return a logical vector?
#'
#' @return sf data frame or logical vector if bool=TRUE
#' @export
#'
#' @importFrom sf st_intersection st_area
geo_trim <- function(from, to, thresh = 0.01, bool = FALSE){
  
  ints <- sf::st_intersection(x = from, y = to)
  
  area <- sf::st_area(from)
  areaints <- sf::st_area(ints)
  keep <- as.numeric(areaints/area) > thresh
  
  if(bool){
    return(keep)
  }
  
  from <- from[keep,]
  
  return(from)
}