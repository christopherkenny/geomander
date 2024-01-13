#' Add Edges to an Adjacency List
#'
#' @param adj list of adjacent precincts
#' @param v1 vector of vertex identifiers for the first vertex. Can be an
#'   integer index or a value to look up in `ids`, if that argument is provided.
#'   If more than one identifier is present, connects each to corresponding
#'   entry in v2.
#' @param v2 vector of vertex identifiers for the second vertex. Can be an
#'   integer index or a value to look up in `ids`, if that argument is provided.
#'   If more than one identifier is present, connects each to corresponding
#'   entry in v2.
#' @param ids A vector of identifiers which is used to look up the row indices
#'   for the vertices.  If provided, the entries in `v1` and `v2` must match
#'   exactly one entry in `ids`. 
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
#' 
#' add_edge(adj, 2, 3)
#' add_edge(adj, "West Haverstraw", "Stony Point", towns$MUNI)
add_edge <- function(adj, v1, v2, ids = NULL, zero = TRUE) {
  if (length(v1) != length(v2)) {
    cli::cli_abort('{.arg v1} and {.arg v2} lengths are different.')
  }

  zero <- as.integer(zero)
  
  matched = match_vtxs(adj, v1, v2, ids)
  v1 <- matched$v1
  v2 <- matched$v2

  for (i in seq_along(v1)) {
    adj[[v1[i]]] <- c(adj[[v1[i]]], v2[i] - zero)
    adj[[v2[i]]] <- c(adj[[v2[i]]], v1[i] - zero)
  }

  adj
}

#' Subtract Edges from an Adjacency List
#'
#' @param adj list of adjacent precincts
#' @param v1 vector of vertex identifiers for the first vertex. Can be an
#'   integer index or a value to look up in `ids`, if that argument is provided.
#'   If more than one identifier is present, disconnects each to corresponding
#'   entry in v2, if an edge exists.
#' @param v2 vector of vertex identifiers for the second vertex. Can be an
#'   integer index or a value to look up in `ids`, if that argument is provided.
#'   If more than one identifier is present, disconnects each to corresponding
#'   entry in v2, if an edge exists.
#' @param ids A vector of identifiers which is used to look up the row indices
#'   for the vertices.  If provided, the entries in `v1` and `v2` must match
#'   exactly one entry in `ids`. 
#' @param zero boolean, TRUE if `adj` is zero indexed. False if one indexed.
#'
#' @export
#' @md
#' @return adjacency list.
#'
#' @concept fix
#' @examples
#' data(towns)
#' adj <- adjacency(towns)
#'
#' subtract_edge(adj, 2, 3)
#' subtract_edge(adj, "West Haverstraw", "Stony Point", towns$MUNI)
subtract_edge <- function(adj, v1, v2, ids = NULL, zero = TRUE) {
  if (length(v1) != length(v2)) {
    cli::cli_abort('{.arg v1} and {.arg v2} lengths are different.')
  }

  zero <- as.integer(zero)
  
  matched = match_vtxs(adj, v1, v2, ids)
  v1 <- matched$v1
  v2 <- matched$v2

  for (i in seq_along(v1)) {
    adj[[v1[i]]] <- setdiff(adj[[v1[i]]], v2[i] - zero)
    adj[[v2[i]]] <- setdiff(adj[[v2[i]]], v1[i] - zero)
  }

  adj
}

# Helper to look up v1 and v2 in ids
match_vtxs <- function(adj, v1, v2, ids = NULL) {
  if (!is.null(ids)) {
    if (length(adj) != length(ids)) {
      cli::cli_abort('{.arg ids} must be the same length as {.arg adj}.', 
                     call=parent.frame())
    }
    
    lv1 <- lapply(v1, function(x) which(x == ids))
    lv2 <- lapply(v2, function(x) which(x == ids))
    
    if (any(lengths(lv1) > 1) || any(lengths(lv2) > 1)) {
      cli::cli_abort(
        c('Provided {.arg ids} are not unique:',
          'i'='Duplicates: {c(v1[lengths(lv1) > 1], v2[lengths(lv2) > 1])}'),
        call=parent.frame()
      )
    }
    if (any(lengths(lv1) == 0) || any(lengths(lv2) == 0)) {
      cli::cli_abort(
        c('Some values in {.arg v1} and {.arg v2} are not in {.arg ids}:',
          'i'='Missing: {c(v1[lengths(lv1) == 0], v2[lengths(lv2) == 0])}'),
        call=parent.frame()
      )
    }
    
    v1 <- unlist(lv1)
    v2 <- unlist(lv2)
  }
  
  list(v1 = v1, v2 = v2)
}

#' Suggest Neighbors for Lonely Precincts
#'
#' For precincts which have no adjacent precincts, this suggests the nearest precinct
#' as a friend to add. This is useful for when a small number of precincts are disconnected
#' from the remainder of the geography, such as an island.
#'
#' @param shp an sf shapefile
#' @param adj an adjacency list
#' @param idx Optional. Which indices to suggest neighbors for. If blank, suggests for those
#' with no neighbors.
#' @param neighbors number of neighbors to suggest
#'
#' @concept fix
#'
#' @return tibble with two columns of suggested rows of shp to connect in adj
#' @export
#'
#' @examples
#' library(dplyr)
#' data(va18sub)
#' va18sub <- va18sub %>% filter(!VTDST %in% c('000516', '000510', '000505', '000518'))
#' adj <- adjacency(va18sub)
#' suggests <- suggest_neighbors(va18sub, adj)
#' adj <- adj %>% add_edge(v1 = suggests$x, v2 = suggests$y)
#'
suggest_neighbors <- function(shp, adj, idx, neighbors = 1) {
  if (missing(idx)) {
    idx <- which(lengths(adj) == 0)
  }

  cents <- st_centerish(shp)

  out <- tibble()
  for (i in idx) {
    nn <- nn_geos(x = cents[i, ], y = cents[-i, ], k = neighbors)
    for (j in seq_along(nn)) {
      if (nn[j] >= i) {
        nn[j] <- nn[j] + 1
      }
    }
    out <- bind_rows(out, tibble(x = i, y = nn))
  }

  out
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
#' sf_rook <- lapply(sf::st_relate(towns, pattern = 'F***1****'), function(x) {
#'   x - 1L
#' })
#' compare_adjacencies(rook, sf_rook, zero = FALSE)
compare_adjacencies <- function(adj1, adj2, shp, zero = TRUE) {
  if (missing(adj1) | missing(adj2)) {
    cli::cli_abort('Please provide an argument to both {.arg adj1} and {.arg adj2}.')
  }

  if (length(adj1) != length(adj2)) {
    cli::cli_abort('{.arg adj1} and {.arg adj2} have different lengths.')
  }

  ret <- tibble()
  for (i in seq_along(adj1)) {
    temp1 <- tibble(
      x = i, y = adj1[[i]][which(!(adj1[[i]] %in% adj2[[i]]))],
      from = 1
    )
    temp2 <- tibble(
      x = i, y = adj2[[i]][which(!(adj2[[i]] %in% adj1[[i]]))],
      from = 2
    )

    ret <- bind_rows(ret, temp1, temp2)
  }


  if (nrow(ret) > 0 & zero) {
    ret$y <- ret$y + 1
  }

  ret$relation <- NA_character_
  ret$class <- NA_character_


  if (!missing(shp)) {
    if (nrow(ret) > 1) {
      for (i in 1:nrow(ret)) {
        temp1 <- shp %>% slice(ret$x[i])
        temp2 <- shp %>% slice(ret$y[i])

        ret$relation[i] <- sf::st_relate(temp1, temp2)
        suppressWarnings(ret$class[i] <- class(sf::st_geometry(sf::st_intersection(
          shp[ret$x[i], ],
          shp[ret$y[i], ]
        )))[1])
      }
    }
  }

  ret
}


#' Build Adjacency List
#'
#' This mimics redist's redist.adjacency using GEOS to create the patterns, rather than sf.
#' This is faster than that version, but forces projections.
#'
#' @param shp sf dataframe
#' @templateVar epsg TRUE
#' @template template
#'
#' @return list with nrow(shp) entries
#' @export
#'
#' @concept fix
#'
#' @examples
#' data(precincts)
#' adj <- adjacency(precincts)
#'
adjacency <- function(shp, epsg = 3857) {
  if (!inherits(shp, 'sf')) {
    cli::cli_abort('Input to {.arg shp} must be an sf dataframe.')
  }

  shp <- make_planar_pair(shp, epsg = epsg)$x

  adj_geos(shp)
}
