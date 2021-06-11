#' Match Across Geographic Layers
#'
#' @param from smaller geographic level to match up from
#' @param to larger geographic level to be matched to
#' @param method string from center, centroid, point, or area for matching method 
#' @param tiebreaker Should ties be broken? boolean. If FALSE, precincts with no 
#' matches get value -1 and precincts with multiple matches get value -2.
#'
#' @return Integer Vector of matches length(to) with values in 1:nrow(from)
#' @export
#' @importFrom sf st_centroid st_point_on_surface st_nearest_feature st_intersection st_make_valid
#' 
#' @concept estimate
#' 
#' @examples
#' library(dplyr)
#' data(checkerboard)
#' counties <- sf::st_as_sf(as.data.frame(rbind(sf::st_union(checkerboard %>% filter(i < 4)), 
#' sf::st_union(checkerboard %>% filter(i >= 4)) )))
#' 
#' geo_match(from = checkerboard, to = counties)
#' geo_match(from = checkerboard, to = counties, method = 'area')
#' 
geo_match <- function(from, to, method = 'center', tiebreaker = TRUE){
  
  match.arg(arg = method, choices = c('center', 'centroid', 'point', 'area'))
  
  if(missing(from)){
    stop('Please provide an argument to "from".')
  }
  if(missing(to)){
    stop('Please provide an argument to "to".')
  }
  
  if(method %in% c('center', 'centroid', 'point')){
    if(method == 'center'){
      op <- st_centerish
    } else if(method == 'centroid'){ 
      op <- sf::st_centroid
    } else {
      op <- sf::st_point_on_surface
    }
    suppressMessages(
    suppressWarnings( pts <- op(from))
    )
    
    suppressMessages(ints <- st_intersects(pts, to))
    if(any(lengths(ints) != 1 )){
      idx <- which(lengths(ints) != 1)
      
      if(tiebreaker){
        suppressMessages(
        suppressWarnings(nnb <- st_nearest_feature(x = st_centroid(from[idx,]), y = st_centroid(to)))
        )
        for(i in 1:length(idx)){
            ints[[ idx[i] ]] <- nnb[i]
          }
        } else{
        
        for(i in 1:length(ints)){
          if(length(ints[[i]]) == 0){
            ints[[i]] <- -1L
          }
          if(length(ints[[i]]) > 1){
            ints[[i]] <- -2L
          }
        }
        
      }
    }
  } else{
    to <- to %>% mutate(toid = row_number())
    from <- from %>% mutate(fromid = row_number())
    from <- st_make_valid(from)
    suppressWarnings(suppressMessages(all_inted <- st_intersection(x = to, y = from)))
    suppressMessages(all_inted$area <- as.numeric(st_area(st_make_valid(all_inted))))
    ints <- all_inted %>% 
      group_by(fromid) %>% 
      slice(which.max(area)) %>% 
      ungroup()
    
    ints <- tibble(fromid = 1:nrow(from)) %>% 
      left_join(ints, by = 'fromid') %>% 
      pull(toid)
    
    if(any(is.na(ints))){
      idx <- which(is.na(ints))
      
      if(tiebreaker){
        suppressMessages(
          suppressWarnings(nnb <- st_nearest_feature(x = st_centroid(from[idx,]), y = st_centroid(to)))
        )
        for(i in 1:length(idx)){
          ints[idx[i]] <- nnb[i]
        }
      }
    }
  }
  
  return(as.integer(ints))
}

globalVariables(c('fromid', 'toid', 'area'))