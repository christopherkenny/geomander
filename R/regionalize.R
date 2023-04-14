#' Estimate Regions by Geographic Features
#'
#' This offers a basic method for dividing a shape into separate pieces
#'
#' @param shp `sf` tibble to estimate regions for
#' @param lines `sf` tibble which divides `shp` into regions
#' @param adj adjacency graph
#' @templateVar epsg TRUE
#' @template template
#'
#' @return integer vector of regions with `nrow(shp)` entries
#' @export
#'
#' @examples
#' data(towns)
#' # make some weird roadlike feature passing through the towns
#' lines <- sf::st_sfc(sf::st_linestring(sf::st_coordinates(sf::st_centroid(towns))),
#'   crs = sf::st_crs(towns)
#' )
#' regionalize(towns, lines)
regionalize <- function(shp, lines, adj = adjacency(shp), epsg = 3857) {
  pairs <- make_planar_pair(shp, lines, epsg = epsg)
  shp <- pairs$x
  lines <- pairs$y

  ec <- edge_center(shp, adj)
  breaks <- geos::geos_intersects_any(ec, lines)
  ec_sub <- dplyr::as_tibble(ec)[breaks, 1:2]

  for (i in seq_len(nrow(ec_sub))) {
    adj <- subtract_edge(adj, ec_sub$i[i], ec_sub$j[i])
  }

  contiguity(adj, rep(0, nrow(shp)))
}


edge_center <- function(shp, adj) {
  centers <- geos::geos_centroid(shp)
  nb <- lapply(adj, function(x) {
    x + 1L
  })
  edgedf <- tibble(
    start = rep(seq_along(nb), lengths(nb)),
    finish = unlist(nb)
  )

  edgedf <- edgedf %>%
    dplyr::rowwise() %>%
    dplyr::mutate(i = min(.data$start, .data$finish), j = max(.data$start, .data$finish)) %>%
    dplyr::select(.data$i, .data$j)
  edgedf <- edgedf[!duplicated(edgedf), ]


  edgedf %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geometry = geos::geos_make_linestring(
      x = c(geos::geos_x(centers[[.data$i]]), geos::geos_x(centers[[.data$j]])),
      y = c(geos::geos_y(centers[[.data$i]]), geos::geos_y(centers[[.data$j]])),
      crs = sf::st_crs(shp)
    )) %>%
    ungroup() %>%
    sf::st_as_sf()
}

# region_voroni_method <- function(from, to) {
#   z <- sf::st_union(to)
#   vor <- st_voronoi(from)
# }
