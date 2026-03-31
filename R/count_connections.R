#' Count How Often Pairs of Units Share a District
#'
#' Summarize a district-membership matrix into pairwise co-assignment counts.
#'
#' @param dm District membership matrix, typically with one row per unit and one
#'   column per plan or draw.
#' @param normalize Logical. If `TRUE`, divide counts by the number of columns in
#'   `dm`.
#'
#' @return tibble in long form with columns `x`, `y`, and `fill`, where `fill`
#'   stores the count or proportion of shared assignments.
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

  tidyr::expand_grid(x = 1:nrow(dm), y = 1:nrow(dm)) |>
    dplyr::mutate(fill = c(mat))
}
