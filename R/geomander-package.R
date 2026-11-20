## usethis namespace: start
#' @useDynLib geomander, .registration = TRUE
## usethis namespace: end
NULL

# importing from sf loads it with geomander, so sf S3 methods (e.g. rbind)
# work on the package's sf datasets
#' @importFrom sf st_geometry
#' @importFrom rlang .env .data :=
#' @importFrom Rcpp evalCpp
NULL
