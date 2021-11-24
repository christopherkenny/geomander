nn_geos <- function(x, y, k = 1) {
  dists <- geos::geos_distance(x, y)
  which(dists %in% min_k(dists, k))
}

min_k <- function(v, k) {
  sort(v)[seq_len(k)]
}

max_k <- function(v, k) {
  sort(v, decreasing = TRUE)[seq_len(k)]
}