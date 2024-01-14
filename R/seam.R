#' Remove Edges along a Boundary
#'
#' @param adj zero indexed adjacency graph
#' @param shp tibble where admin column is found
#' @param admin quoted name of administrative unit column
#' @param seam units to rip the seam between by removing adjacency connections
#' @templateVar epsg TRUE
#' @template template
#'
#' @return adjacency list
#' @export
#'
#' @concept seam
#'
#' @examples
#' data('rockland')
#' data('orange')
#' data('nrcsd')
#'
#' o_and_r <- rbind(orange, rockland)
#' o_and_r <- o_and_r %>%
#'   geo_filter(nrcsd) %>%
#'   geo_trim(nrcsd)
#' adj <- adjacency(o_and_r)
#'
#' seam_rip(adj, o_and_r, 'county', c('071', '087'))
#'
seam_rip <- function(adj, shp, admin, seam, epsg = 3857) {
  shp <- make_planar_pair(shp, epsg = epsg)$x

  admin <- shp[[admin]]

  if (length(seam) < 2) {
    cli::cli_abort('{.arg seam} must have at least two entries.')
  }

  combs <- utils::combn(seam, 2)

  for (i in seq_len(ncol(combs))) {
    admin_a <- which(admin == combs[1, i])
    admin_b <- which(admin == combs[2, i])

    seams <- tidyr::expand_grid(admin_a, admin_b)
    adj <- subtract_edge(adj, seams$admin_a, seams$admin_b)
  }

  adj
}

#' Filter Shape to Geographies Along Border
#'
#' @param adj zero indexed adjacency graph
#' @param shp tibble to subset and where admin column is found
#' @param admin quoted name of administrative unit column
#' @param seam administrative units to filter by
#' @templateVar epsg TRUE
#' @template template
#'
#' @return subset of shp
#' @export
#' @concept seam
#'
#' @examples
#' data('rockland')
#' data('orange')
#' data('nrcsd')
#'
#' o_and_r <- rbind(orange, rockland)
#' o_and_r <- o_and_r %>%
#'   geo_filter(nrcsd) %>%
#'   geo_trim(nrcsd)
#' adj <- adjacency(o_and_r)
#'
#' seam_geom(adj, shp = o_and_r, admin = 'county', seam = c('071', '087'))
#'
seam_geom <- function(adj, shp, admin, seam, epsg = 3857) {
  shp <- make_planar_pair(shp, epsg = epsg)$x

  admin <- shp[[admin]]

  admin_a <- which(admin == seam[1])
  admin_b <- which(admin == seam[2])

  keep_a <- vapply(seq_along(admin_a), function(i) {
    any((adj[[admin_a[i]]] + 1L) %in% admin_b)
  }, FALSE)

  keep_b <- vapply(seq_along(admin_b), function(i) {
    any((adj[[admin_b[i]]] + 1L) %in% admin_a)
  }, FALSE)

  keep <- sort(c(admin_a[keep_a], admin_b[keep_b]))
  shp[keep, ]
}

#' Filter Adjacency to Edges Along Border
#'
#' @param adj zero indexed adjacency graph
#' @param shp tibble to subset and where admin column is found
#' @param admin quoted name of administrative unit column
#' @param seam administrative units to filter by
#' @templateVar epsg TRUE
#' @template template
#'
#' @return subset of adj
#' @export
#' @concept seam
#'
#' @examples
#' data('rockland')
#' data('orange')
#' data('nrcsd')
#'
#' o_and_r <- rbind(orange, rockland)
#' o_and_r <- o_and_r %>%
#'   geo_filter(nrcsd) %>%
#'   geo_trim(nrcsd)
#' adj <- adjacency(o_and_r)
#'
#' seam_adj(adj, shp = o_and_r, admin = 'county', seam = c('071', '087'))
#'
seam_adj <- function(adj, shp, admin, seam, epsg = 3857) {
  shp <- make_planar_pair(shp, epsg = epsg)$x

  admin <- shp[[admin]]

  admin_a <- which(admin == seam[1])
  admin_b <- which(admin == seam[2])

  keep_a <- vapply(seq_along(admin_a), function(i) {
    any((adj[[admin_a[i]]] + 1L) %in% admin_b)
  }, FALSE)

  keep_b <- vapply(seq_along(admin_b), function(i) {
    any((adj[[admin_b[i]]] + 1L) %in% admin_a)
  }, FALSE)

  admin_a <- admin_a[keep_a]
  admin_b <- admin_b[keep_b]

  lapply(
    seq_along(adj),
    function(i) {
      if (i %in% admin_a) {
        x <- adj[[i]]
        return(x[x %in% (admin_b - 1L)])
      } else if (i %in% admin_b) {
        x <- adj[[i]]
        return(x[x %in% (admin_a - 1L)])
      }
      integer()
    }
  )
}

#' Suggest Edges to Connect Two Sides of a Border
#'
#' @param shp sf tibble where admin column is found
#' @param admin quoted name of administrative unit column
#' @param seam administrative units to filter by
#' @templateVar epsg TRUE
#' @template template
#'
#' @return tibble of edges connecting sides of a border
#' @export
#' @concept seam
#'
#' @examples
#' data('rockland')
#' data('orange')
#' data('nrcsd')
#'
#' o_and_r <- rbind(orange, rockland)
#' o_and_r <- o_and_r %>%
#'   geo_filter(nrcsd) %>%
#'   geo_trim(nrcsd)
#' adj <- adjacency(o_and_r)
#'
#' adds <- seam_sew(o_and_r, 'county', c('071', '087'))
#' adj <- adj %>% add_edge(adds$v1, adds$v2)
#'
seam_sew <- function(shp, admin, seam, epsg = 3857) {
  shp <- make_planar_pair(shp, epsg = epsg)$x

  cents <- shp %>%
    geos_centerish()

  vr <- cents %>%
    geos::geos_make_collection() %>%
    geos::geos_voronoi_polygons() %>%
    geos::geos_geometry_n(seq_len(geos::geos_num_geometries(.)))

  vr <- vr[unlist(geos::geos_intersects_matrix(cents, vr))]

  adj <- seam_adj(adj_geos(vr), shp, admin, seam)

  tibble(
    v1 = unlist(lapply(seq_along(adj), function(i) {
      rep(i, length(adj[[i]]))
    })),
    v2 = unlist(adj) + 1L
  ) %>%
    dplyr::filter(.data$v1 < .data$v2)
}
