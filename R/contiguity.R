#' Check Contiguity by Group
#'
#' Identify contiguous sets of units and numbers each set. Can be extended to repeat the procedure
#'  within a subgeography.
#'
#' Given a zero-indexed adjacency list and an array of group identifiers, this
#' returns a tibble which identifies the connected components. The three columns
#' are `group` for the inputted group, `group_number` which uniquely identifies each
#' group as a positive integer, and `component` which identifies the connected
#' component number for each corresponding entry of adjacency and group. If everything
#' is connected within the group, then each element of `component` will be `1`.
#' Otherwise, the largest component is given the value `1`, the next largest `2`,
#' and so on.
#'
#' If nothing is provided to group, it will default to a vector of ones, checking
#' if the adjacency graph is connected.
#'
#' `cct()` is shorthand for creating a table of the component values. If everything
#' is connected within each group, it returns a value of 1. In general, it returns
#' a frequency table of components.
#'
#' `ccm()` is shorthand for getting the maximum component value. It returns the
#' maximum number of  components that a group is broken into.
#' This returns 1 if each group is connected. #'
#'
#' @param adj adjacency list
#' @param group array of group identifiers. Typically district numbers or county names.
#' Defaults to 1 if no input is provided, checking that the adjacency list itself is
#' one connected component.
#'
#' @return tibble with contiguity indicators. Each row is the units of `adj`. Columns include
#'  - `group` Values of the inputted `group` argument. If `group` is not specified, then all values
#'    will be 1. 
#'  - `component` A number for each contiguous set of units within a `group`. If all units within a
#'    `group` are contiguous, all values are 1. If there are two sets, each discontiguous with
#'    the other, the larger one will be numbered 1 and the smaller one will be numbered as 2.
#'
#' @concept fix
#'
#' @export
#' @examples
#' data(checkerboard)
#' adj <- adjacency(checkerboard)
#' # These each indicate the graph is connected.
#' check_contiguity(adj) # all contiguous
#' # If there are two discontiguous groups, there will be 2 values of `component`
#' cct(adj)
#' ccm(adj)
#'
check_contiguity <- function(adj, group) {
  if (missing(adj)) {
    cli::cli_abort('Please provide an argument to {.arg adj}.')
  }
  if (!missing(group)) {
    if (length(adj) != length(group)) {
      cli::cli_abort('{.arg adj} and {.arg group} are different lengths.')
    }
    sorted <- sort(unique(group))
    groups <- match(group, sorted)
  } else {
    group <- 1L
    groups <- rep(1L, length(adj))
  }

  out <- tibble(
    group = group,
    group_number = groups,
    component = contiguity(adj, groups - 1L)
  )

  if (max(out$component) == 1) {
    return(out)
  } else {
    out <- out %>%
      dplyr::group_by(.data$group_number) %>%
      dplyr::mutate(
        component = match(component, as.numeric(names(sort(table(component), decreasing = TRUE))))
    ) %>%
      dplyr::ungroup()
  }
  
  out
}

#' @rdname check_contiguity
#' @export
cct <- function(adj, group) {
  table(check_contiguity(adj = adj, group = group)$component)
}

#' @rdname check_contiguity
#' @export
ccm <- function(adj, group) {
  max(check_contiguity(adj = adj, group = group)$component)
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
#' @templateVar epsg TRUE
#' @template template
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
check_polygon_contiguity <- function(shp, group, epsg = 3857) {
  if (missing(shp)) {
    cli::cli_abort('Please provide an argument to {.arg shp}.')
  }
  if (missing(group)) {
    cli::cli_abort('Please provide an argument to {.arg group}.')
  }

  shp <- make_planar_pair(x = shp, epsg = 3857)$x

  shp <- shp %>%
    dplyr::select({{ group }}) %>%
    sf::st_cast('POLYGON') %>%
    sf::st_as_sf() %>%
    suppressWarnings()

  adj <- adj_geos(shp)

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
#' @templateVar epsg TRUE
#' @template template
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
suggest_component_connection <- function(shp, adj, group, epsg = 3857) {
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

  shp <- make_planar_pair(x = shp, epsg = 3857)$x
  shp <- shp %>% mutate(rownum = row_number())

  out <- tibble()
  for (g in seq_len(length(unique(group)))) {
    sub <- components$component[group == g]
    if (max(sub) > 1) {
      cents <- st_centerish(shp)
      for (c in 1:max(sub)) {
        tempx <- cents[group == g & components$component == c, ]
        tempy <- cents[group == g & components$component != c, ]
        dists <- dist_mat_geos(x = tempx, y = tempy)
        prop <- arrayInd(which.min(dists), dim(dists))
        out <- out %>%
          dplyr::bind_rows(tibble(
            x = tempx$rownum[prop[1, 1]],
            y = tempy$rownum[prop[1, 2]]
          ))
      }
    }
  }

  for (i in seq_len(nrow(out))) {
    if (out[i, 1] > out[i, 2]) {
      temp <- out[i, 1]
      out[i, 1] <- out[i, 2]
      out[i, 2] <- temp
    }
  }

  dplyr::distinct(out)
}
