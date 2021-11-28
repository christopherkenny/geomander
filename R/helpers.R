min_k <- function(v, k) {
  sort(v)[seq_len(k)]
}

max_k <- function(v, k) {
  sort(v, decreasing = TRUE)[seq_len(k)]
}
