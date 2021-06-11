
#' Filter to Intersecting Pieces
#'
#' @param from Required. sf dataframe. the geography to subset
#' @param to Required. sf dataframe. the geography that from must intersect
#' @param bool Optional, defaults to FALSE. Should this just return a logical vector?
#'
#' @return sf data frame or logical vector if bool=TRUE
#' @export
#'
#' @concept datatable
#' @importFrom sf st_intersects st_union
#' @examples \dontrun{
#' data(towns)
#' block <- create_block_table('NY', 'Rockland')
#' geo_filter(block, towns)
#' }
geo_filter <- function(from, to, bool = FALSE){
  
  ints <- sf::st_intersects(x = from, y = st_union(to), sparse = FALSE)
  
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
#' @param thresh Percent as decimal of an area to trim away. Default is .01, which is 1\%.
#' @param bool Optional, defaults to FALSE. Should this just return a logical vector?
#'
#' @return sf data frame or logical vector if bool=TRUE
#' @export
#'
#' @importFrom sf st_intersection st_area st_union
#'
#' @concept datatable
#' @examples \dontrun{
#' # Needs Census Bureau API
#' data(towns)
#' block <- create_block_table('NY', 'Rockland')
#' geo_trim(block, towns, thresh = 0.05)
#' }
#' 
#' \donttest{
#' data(towns)
#' data(rockland)
#' sub <- geo_filter(rockland, towns)
#' rem <- geo_trim(sub, towns, thresh = 0.05)
#' }
geo_trim <- function(from, to, thresh = 0.01, bool = FALSE){

  ints <- sf::st_intersection(x = from, y = st_union(to))
  area <- sf::st_area(from)
  poly <- attr(from, 'row.names') %in% attr(ints, 'row.names')
  areaints <- rep(0, nrow(from))
  areaints[poly] <- st_area(st_make_valid(ints)) #, NA_on_exception = TRUE in valid
  keep <- as.numeric(areaints/area) > thresh
  
  if(bool){
    return(keep)
  }
  
  from <- from[keep,]
  
  return(from)
}