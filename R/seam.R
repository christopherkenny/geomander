#' Remove Edges Across an Administrative Boundary
#'
#' Remove adjacency connections between two or more administrative units, such as
#' counties on opposite sides of a seam.
#'
#' @param adj Zero-indexed adjacency graph.
#' @param shp `sf` object containing the administrative identifier.
#' @param admin Name of the administrative-unit column in `shp`.
#' @param seam Vector of administrative-unit values to separate.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return adjacency list with all cross-seam edges removed
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
#' o_and_r <- o_and_r |>
#'   geo_filter(nrcsd) |>
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

#' Filter Shapes to Units Along a Seam
#'
#' Keep only rows in `shp` that lie on the border between two administrative
#' units and have at least one adjacency connection across that border.
#'
#' @param adj Zero-indexed adjacency graph.
#' @param shp `sf` object containing the administrative identifier.
#' @param admin Name of the administrative-unit column in `shp`.
#' @param seam Length-2 vector of administrative-unit values defining the seam.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return subset of `shp` containing only rows on the selected seam
#' @export
#' @concept seam
#'
#' @examples
#' data('rockland')
#' data('orange')
#' data('nrcsd')
#'
#' o_and_r <- rbind(orange, rockland)
#' o_and_r <- o_and_r |>
#'   geo_filter(nrcsd) |>
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

#' Filter Adjacency to the Edges Along a Seam
#'
#' Keep only the adjacency edges that connect the two sides of a selected seam.
#'
#' @param adj Zero-indexed adjacency graph.
#' @param shp `sf` object containing the administrative identifier.
#' @param admin Name of the administrative-unit column in `shp`.
#' @param seam Length-2 vector of administrative-unit values defining the seam.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return adjacency list containing only cross-seam edges
#' @export
#' @concept seam
#'
#' @examples
#' data('rockland')
#' data('orange')
#' data('nrcsd')
#'
#' o_and_r <- rbind(orange, rockland)
#' o_and_r <- o_and_r |>
#'   geo_filter(nrcsd) |>
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

#' Suggest Edges to Sew a Seam
#'
#' Build a set of candidate cross-seam edges by constructing Voronoi cells from
#' representative points and identifying neighboring cells across the seam.
#'
#' @param shp `sf` object containing the administrative identifier.
#' @param admin Name of the administrative-unit column in `shp`.
#' @param seam Length-2 vector of administrative-unit values defining the seam.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return tibble with columns `v1` and `v2`, suitable for use with [add_edge()]
#' @export
#' @concept seam
#'
#' @examples
#' data('rockland')
#' data('orange')
#' data('nrcsd')
#'
#' o_and_r <- rbind(orange, rockland)
#' o_and_r <- o_and_r |>
#'   geo_filter(nrcsd) |>
#'   geo_trim(nrcsd)
#' adj <- adjacency(o_and_r)
#'
#' adds <- seam_sew(o_and_r, 'county', c('071', '087'))
#' adj <- adj |> add_edge(adds$v1, adds$v2)
#'
seam_sew <- function(shp, admin, seam, epsg = 3857) {
  shp <- make_planar_pair(shp, epsg = epsg)$x

  cents <- shp |>
    geos_centerish()

  vor <- cents |>
    geos::geos_make_collection() |>
    geos::geos_voronoi_polygons()
  vr <- vor |>
    geos::geos_geometry_n(seq_len(geos::geos_num_geometries(geom = vor)))

  vr <- vr[unlist(geos::geos_intersects_matrix(cents, vr))]

  adj <- seam_adj(adj_geos(vr), shp, admin, seam)

  tibble(
    v1 = unlist(lapply(seq_along(adj), function(i) {
      rep(i, length(adj[[i]]))
    })),
    v2 = unlist(adj) + 1L
  ) |>
    dplyr::filter(.data$v1 < .data$v2)
}
