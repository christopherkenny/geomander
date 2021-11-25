#' Check Contiguity by Group
#'
#' @param adj adjacency list
#' @param group array of group identifiers. Typically district numbers or county names.
#'
#' @return tibble with a column for each of inputted group, created group number, and the
#' identified connected component number
#'
#' @concept fix
#'
#' @export
#' @examples
#' data(checkerboard)
#' adj <- adjacency(checkerboard)
#' check_contiguity(adj)
#' 
check_contiguity <- function(adj, group) {
  if (missing(adj)) {
    cli::cli_abort('Please provide an argument to {.arg adj}.')
  }
  if (!missing(group)) {
    if (length(adj) != length(group)) {
      cli::cli_abort('{.arg adj} and {.arg group} are different lengths.')
    }
    groups <- rep(0, length(group))
    sorted <- sort(unique(group))
    for (i in 1:length(group)) {
      groups[i] <- which(sorted == group[i])
    }
  } else {
    group <- 1L
    groups <- rep(1L, length(adj))
  }

  out <- tibble(group = group, 
                group_number = groups, 
                component = contiguity(adj, groups))
  
  if (max(out$component) == 1) {
    return(out)
  } else {
    out <- out %>%
      dplyr::group_by(.data$group_number) %>%
      dplyr::mutate(ranks = list(as.numeric(names(sort(table(component),
                                                       decreasing = TRUE))))) %>%
      dplyr::rowwise() %>% 
      dplyr::mutate(component = which(component == .data$ranks)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-.data$ranks)
  }
  
  out
}


#' Check Polygon Contiguity
#'
#' Cast `shp` to component polygons, build the adjacency, and check the contiguity.
#' Avoids issues where a precinct is actually a multipolygon
#'
#' @param shp An sf data frame
#' @param group unquoted name of group identifier in shp.
#' Typically, this is district assignment. If you're looking for dis-contiguous precincts,
#' this should be a row number.
#'
#' @return tibble with a column for each of inputted group, created group number, and the
#' identified connected component number
#'
#' @concept fix
#'
#' @export
#'
#' @examples
#' data(checkerboard)
#' check_polygon_contiguity(checkerboard, i)
check_polygon_contiguity <- function(shp, group) {
  if (missing(shp)) {
    cli::cli_abort('Please provide an argument to {.arg shp}.')
  }
  if (missing(group)) {
    cli::cli_abort('Please provide an argument to {.arg group}.')
  }

  shp <- shp %>%
    dplyr::select({{ group }}) %>%
    sf::st_cast('POLYGON') %>%
    sf::st_as_sf() %>%
    suppressWarnings()

  adj <- adjacency(shp)

  check_contiguity(adj = adj, group = shp %>%
    sf::st_drop_geometry() %>%
    dplyr::pull({{ group }}))
}

#' Suggest Connections for Disconnected Groups
#'
#' Suggests nearest neighbors for connecting a disconnected group.
#'
#' @param shp An sf data frame
#' @param adj adjacency list
#' @param group array of group identifiers. Typically district numbers or county names.
#' Defaults to rep(1, length(adj)) if missing.
#'
#' @return tibble with two columns of suggested rows of shp to connect in adj
#' @export
#'
#' @concept fix
#'
#' @examples
#' library(dplyr)
#' data(checkerboard)
#' checkerboard <- checkerboard %>% filter(i != 1, j != 1)
#' adj <- adjacency(checkerboard)
#' suggest_component_connection(checkerboard, adj)
#' 
suggest_component_connection <- function(shp, adj, group) {
  if (missing(shp)) {
    cli::cli_abort('Please provide an argument to {.arg shp}')
  }
  if (missing(adj)) {
    cli::cli_abort('Please provide an argument to {.arg adj}.')
  }
  if (missing(group)) {
    group <- rep(1, length(adj))
  }

  components <- check_contiguity(adj = adj, group = group)

  shp <- shp %>% mutate(rownum = row_number())

  out <- tibble()
  for (g in 1:length(unique(group))) {
    sub <- components$component[group == g]
    if (max(sub) > 1) {
      cents <- st_centerish(shp)
      for (c in 1:max(sub)) {
        tempx <- cents[group == g & components$component == c, ]
        # shp %>% filter(group == g, components$component == c)
        tempy <- cents[group == g & components$component != c, ]
        # shp %>% filter(group == g, components$component != c)
        dists <- dist_mat_geos(x = tempx, y = tempy)
        prop <- arrayInd(which.min(dists), dim(dists))
        out <- out %>% 
          bind_rows(tibble(x = tempx$rownum[prop[1, 1]], 
                           y = tempy$rownum[prop[1, 2]])
          )
      }
    }
  }

  for (i in 1:nrow(out)) {
    if (out[i, 1] > out[i, 2]) {
      temp <- out[i, 1]
      out[i, 1] <- out[i, 2]
      out[i, 2] <- temp
    }
  }

  distinct(out)
}
