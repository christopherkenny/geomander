#' Add Edges to an Adjacency List
#'
#' @param adjacency list of adjacent precincts
#' @param v1 integer or integer array for first vertex to connect. 
#' If array, connects each to corresponding entry in v2.
#' @param v2 integer or integer array for second vertex to connect.
#' If array, connects each to corresponding entry in v1.
#' @param zero boolean, TRUE if the list is zero indexed. False if one indexed.
#'
#' @return adjacency list.
#' @export
#'
#' @examples
add_edge <- function(adjacency, v1, v2, zero = TRUE){
  if(length(v1) != length(v2)){
    stop('v1 and v2 lengths are different.')
  }
  
  for(i in 1:length(v1)){
    if(zero){
      adjacency[[v1[i]]] <- c(adjacency[[v1[i]]], v2[i]-1)
      adjacency[[v2[i]]] <- c(adjacency[[v2[i]]], v1[i]-1)
    } else {
      adjacency[[v1[i]]] <- c(adjacency[[v1[i]]], v2[i])
      adjacency[[v2[i]]] <- c(adjacency[[v2[i]]], v1[i])
    }
  }

  return(adjacency)
}

#' Suggest Neighbors for Lonely Precincts
#'
#' For precincts which have no adjacent precincts, this suggests the nearest precinct
#' as a friend to add. This is useful for when a small number of precincts are disconnected
#' from the remainder of the geography, such as an island.
#'
#' @param shp an sf shapefile
#' @param adjacency an adjacency list
#' @param idx Optional. Which indices to suggest neighbors for. If blank, suggests for those
#' with no neighbors.
#' @param neighbors number of neighbors to suggest
#'
#' @return
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom nngeo st_nn
#' @importFrom tibble tibble
#' @examples
suggest_neighbors <- function(shp, adjacency, idx, neighbors = 1){
  if(missing(idx)){
    idx <- which(lengths(adjacency) == 0)
  }
  
  out <- tibble()
  for(i in idx){
    
    nn <- suppressMessages(st_nn(x = shp[i,], y = shp[-i,], k = neighbors) %>% unlist())
    for(j in 1:length(nn)){
      if(nn[j] >= i){
        nn[j] <- nn[j] + 1
      }
    }
    
    
    out <- bind_rows(out, tibble(x = i, y = nn))
  }
  
  return(out)
  
}