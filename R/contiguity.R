#' Check Contiguity by Group
#'
#' @param adjacency adjacency list
#' @param group array of group identifiers. Typically district numbers or county names.
#'
#' @return tibble with a column for each of inputted group, created group number, and the 
#' identified connected component number
#' 
#' @concept fix
#' 
#' @export
#' @importFrom tibble tibble
#' @examples 
#' data(checkerboard)
#' adj <- adjacency(checkerboard)
#' check_contiguity(adj)
#' 
check_contiguity <- function(adjacency, group){
  if(missing(adjacency)){
    stop('Please provide an argument to adjacency.')
  }
  if(!missing(group)){
    if(length(adjacency) != length(group)){
      stop('Adjacency and group are different lengths.')
    }
    groups <- rep(0, length(group))
    sorted <- sort(unique(group))
    for(i in 1:length(group)){
      groups[i] <- which(sorted == group[i])
    }
  } else {
    group <- 1L
    groups <- rep(1L, length(adjacency))
  }
  
  out <- tibble(group = group, group_number = groups, component = contiguity(adjacency, groups))
  
  return(out) 
}

#' Suggest Connections for Disconnected Groups
#' 
#' Suggests nearest neighbors for connecting a disconnected group.
#'
#' @param shp An sf data frame
#' @param adjacency adjacency list
#' @param group array of group identifiers. Typically district numbers or county names. 
#' Defaults to rep(1, length(adjacency)) if missing.
#'
#' @return tibble with two columns of suggested rows of shp to connect in adj
#' @export
#'
#' @importFrom dplyr row_number distinct filter mutate 
#' @importFrom sf st_distance
#'
#' @concept fix
#'
#' @examples 
#' data(checkerboard)
#' checkerboard <- checkerboard %>% filter(i != 1, j != 1)
#' adj <- adjacency(checkerboard)
#' suggest_component_connection(checkerboard, adj)
#' 
suggest_component_connection <- function(shp, adjacency, group){
  if(missing(shp)){
    stop('Please provide an argument to shp')
  }
  if(missing(adjacency)){
    stop('Please provide an argument to adjacency.')
  }
  if(missing(group)){
    group <- rep(1, length(adjacency))
  }
  
  components <- check_contiguity(adjacency = adjacency, group = group)
  
  shp <- shp %>% mutate(rownum = row_number())
  
  out <- tibble()
  for(g in 1:length(unique(group))){
    sub <- components$component[group == g]
    if(max(sub) > 1){
      cents <- st_centerish(shp)
      for(c in 1:max(sub)){
        tempx <- cents[group == g & components$component == c,]
          #shp %>% filter(group == g, components$component == c)
        tempy <- cents[group == g & components$component != c,]
          #shp %>% filter(group == g, components$component != c)
        dists <- sf::st_distance(x = tempx, y = tempy)
        prop <- arrayInd(which.min(dists), dim(dists))
        out <- out %>% bind_rows(tibble(x = tempx$rownum[prop[1,1]], y = tempy$rownum[prop[1,2]]))
      }
    }
  }
  
  for(i in 1:nrow(out)){
    if(out[i, 1] > out[i,2]){
      temp <- out[i, 1]
      out[i, 1] <- out[i, 2]
      out[i, 2] <- temp
    }
  }
  
  return(distinct(out))
  
}


