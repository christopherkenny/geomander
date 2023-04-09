#' Count Times Precincts are Connected
#'
#' @param dm district membership matrix
#' @param normalize Whether to normalize all values by the number of columns.
#'
#' @return matrix with the number of connections between precincts
#' @export
#' @concept leftover
#' @examples
#' set.seed(1)
#' dm <- matrix(sample(1:2, size = 100, TRUE), 10)
#' count_connections(dm)
count_connections <- function(dm, normalize = FALSE) {
  mat <- countconnections(dm)
  mat <- mat + t(mat)
  diag(mat) <- rep(ncol(dm), nrow(dm))

  if (normalize) {
    mat <- mat / ncol(dm)
  }

  tidyr::expand_grid(x = 1:nrow(dm), y = 1:nrow(dm)) %>%
    dplyr::mutate(fill = c(mat))
}
