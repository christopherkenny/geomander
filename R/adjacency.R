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
#' @concept fix
#'
#' @examples 
#' data(towns)
#' adj <- adjacency(towns)
#' add_edge(adj, 2, 3)
#' 
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
#' @concept fix
#' 
#' @return tibble with two columns of suggested rows of shp to connect in adj
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom nngeo st_nn
#' @importFrom tibble tibble
#' @examples 
#' library(dplyr)
#' data(va18sub)
#' va18sub <- va18sub %>% filter(!VTDST %in% c('000516', '000510', '000505', '000518'))
#' adj <- adjacency(va18sub)
#' suggests <- suggest_neighbors(va18sub, adj)
#' adj <- adj %>% add_edge(v1 = suggests$x, v2 = suggests$y)
#' 
suggest_neighbors <- function(shp, adjacency, idx, neighbors = 1){
  if(missing(idx)){
    idx <- which(lengths(adjacency) == 0)
  }
  
  cents <- st_centerish(shp)
  
  out <- tibble()
  for(i in idx){
    
    nn <- suppressMessages(st_nn(x = cents[i,], y = cents[-i,], k = neighbors) %>% unlist())
    for(j in 1:length(nn)){
      if(nn[j] >= i){
        nn[j] <- nn[j] + 1
      }
    }
    
    
    out <- bind_rows(out, tibble(x = i, y = nn))
  }
  
  return(out)
  
}




#' Compare Adjacency Lists
#'
#' @param adj1 Required. A first adjacency list.
#' @param adj2 Required. A second adjacency list.
#' @param shp shapefile to compare intersection types.
#' @param zero Boolean. Defaults to TRUE. Are adj1 and adj2 zero indexed?
#'
#' @return tibble with row indices to compare, and optionally columns which describe the 
#' DE-9IM relationship between differences.
#' @export
#' 
#' @concept fix
#' 
#' @examples 
#' data(towns)
#' rook <- adjacency(towns)
#' sf_rook <- lapply(sf::st_relate(towns, pattern = 'F***1****'), function(x){x-1L})
#' compare_adjacencies(rook, sf_rook, zero = FALSE)
#' 
compare_adjacencies <- function(adj1, adj2, shp, zero = TRUE){
  
  if(missing(adj1) | missing(adj2)){
    stop('Please provide an argument to both adj1 and adj2.')
  }
  
  if(length(adj1) != length(adj2)){
    stop('Adjacencies have different lengths.')
  }
  
  ret <- tibble()
  for(i in 1:length(adj1)){

    temp1 <- tibble(x = i, y = adj1[[i]][which(!(adj1[[i]] %in% adj2[[i]]))],
                    from = 1)
    temp2 <- tibble(x = i, y = adj2[[i]][which(!(adj2[[i]] %in% adj1[[i]]))],
                    from = 2)
    
    ret <- bind_rows(ret, temp1, temp2)
  }
  
  
  if(nrow(ret) > 0 & zero){
    ret$y <- ret$y + 1
  }
  
  ret$relation <- NA_character_
  ret$class <- NA_character_
  
  
  if(!missing(shp)){
    if(nrow(ret) > 1){
      for(i in 1:nrow(ret)){
        temp1 <- shp %>% slice(ret$x[i])
        temp2 <- shp %>% slice(ret$y[i])
        
        ret$relation[i] <- st_relate(temp1, temp2)
        suppressWarnings(ret$class[i] <- class(st_geometry(st_intersection(shp[ret$x[i],], 
                                                          shp[ret$y[i],])))[1])
      }
    }
  }
  

  
  return(ret)
}


#' Build Adjacency List
#' 
#' This is similar to the old version of redist's adjacency function which has been
#' replaced by sf internals. This is faster but less reliable. This wraps spdep::poly2nb()
#' and replaces the indicator for no adjacent precincts with a integer(0)
#'
#' @param shp sf dataframe
#' @param zero Default is TRUE for zero indexed. FALSE gives a one indexed adjacency list.
#' @param rook Default is TRUE for rook adjacency. FALSE gives queen adjacency
#'
#' @return list with nrow(shp) entries
#' @export
#' 
#' @importFrom spdep poly2nb
#' 
#' @concept fix
#' 
#' @examples
#' data(precincts)
#' adj <- adjacency(precincts)
adjacency <- function(shp, zero = TRUE, rook = TRUE){
  
  if(!inherits(shp, 'sf')){
    stop('Input to `shp` must be an sf dataframe.')
  }
  
  adj <-spdep::poly2nb(shp, queen = !rook)
  
  if(zero){
    adj <- lapply(adj, function(x){x-1L})
    adj <- lapply(adj, function(x){if(all(x == -1L)){integer(0)} else {x} })
  } else {
    adj <- lapply(adj, function(x){x-0L})
    adj <- lapply(adj, function(x){if(all(x == -1L)){integer(0)} else {x} })
  }

  return(adj)
}