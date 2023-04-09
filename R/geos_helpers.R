largest_intersection_geos <- function(x, y) {
  l <- geos::geos_intersects_matrix(x, y)

  a <- lapply(seq_along(l), function(i) {
    geos::geos_area(geos::geos_intersection(x[[i]], y[[l[[i]]]]))
  })

  vapply(seq_along(l), function(i) {
    o <- l[[i]][which.max(a[[i]])]
    if (length(o) == 0) {
      o <- NA_real_
    }
    o
  }, 0)
}

area_intersection_geos <- function(x, y) {
  l <- geos::geos_intersects_matrix(x, y)

  a <- lapply(seq_along(l), function(i) {
    geos::geos_area(geos::geos_intersection(x[[i]], y[[l[[i]]]])) / geos::geos_area(y[[l[[i]]]])
  })

  lapply(
    seq_along(a),
    function(i) {
      x <- a[[i]]
      names(x) <- as.character(l[[i]])
      x
    }
  )
}

nn_geos <- function(x, y, k = 1) {
  dists <- geos::geos_distance(x, y)
  which(dists %in% min_k(dists, k))[seq_len(k)]
}

dist_mat_geos <- function(x, y) {
  out <- matrix(0, nrow(x), nrow(y))

  x <- geos::geos_centroid(x)
  y <- geos::geos_centroid(y)


  for (i in seq_along(x)) {
    out[i, ] <- geos::geos_distance(x[[i]], y)
  }

  out
}

bbox_geos <- function(x) {
  geos::geos_envelope(geos::geos_unary_union(geos::geos_make_collection(x)))
}

adj_geos <- function(shp) {
  shp <- geos::as_geos_geometry(shp)
  nby <- geos::geos_strtree_query(geos::geos_strtree(shp), shp)

  lapply(
    seq_len(length(shp)),
    function(i) {
      x <- geos::geos_relate(shp[[i]], shp[[nby[[i]]]])
      nby[[i]][geos::geos_relate_pattern_match(x, 'F***1****') |
        geos::geos_relate_pattern_match(x, '2121**2*2')] - 1L
    }
  )
}
