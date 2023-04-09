## usethis namespace: start
#' @importFrom tibble tibble
#' @useDynLib geomander, .registration = TRUE
## usethis namespace: end
NULL

#' @importFrom sf st_geometry<-
#' @importFrom dplyr bind_rows row_number distinct filter mutate summarize arrange
#' @importFrom dplyr select all_of any_of left_join .data
#' @importFrom dplyr rename starts_with n desc bind_cols
#' @importFrom dplyr group_by ungroup slice pull
#' @importFrom ggplot2 ggsave geom_sf labs theme_void scale_fill_brewer ggplot aes
#' @importFrom ggplot2 geom_sf_text
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect
#' @importFrom readr read_csv write_csv
#' @importFrom rlang .env .data :=
#' @importFrom Rcpp evalCpp
NULL

globalVariables(c('.'))
