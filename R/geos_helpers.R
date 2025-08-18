largest_intersection_geos <- function(x, y) {

  n_x <- length(x)
  result <- integer(n_x)
  
  intersect_matrix <- geos::geos_intersects_matrix(x, geos::geos_strtree(y))
  
  # Handle cases with no intersections
  no_intersect <- lengths(intersect_matrix) == 0
  result[no_intersect] <- NA_integer_
  
  # Handle cases with exactly one intersection (no area computation needed)
  single_intersect <- lengths(intersect_matrix) == 1
  result[single_intersect] <- vapply(intersect_matrix[single_intersect], function(v) v[[1L]], double(1))
  
  # Handle cases with multiple intersections
  multi_intersect <- lengths(intersect_matrix) > 1
  if (any(multi_intersect)) {
    multi_indices <- which(multi_intersect)
    
    for (i in multi_indices) {
      candidates <- intersect_matrix[[i]]
      intersections <- geos::geos_intersection(x[[i]], y[candidates])
      areas <- geos::geos_area(intersections)
      result[i] <- candidates[which.max(areas)]
    }
  }
  
  result
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
