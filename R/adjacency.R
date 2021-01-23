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