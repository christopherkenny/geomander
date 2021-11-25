## usethis namespace: start
#' @importFrom tibble tibble
#' @useDynLib geomander, .registration = TRUE
## usethis namespace: end
NULL

#' @importFrom sf st_geometry<- st_distance st_crs st_transform st_area
#' @importFrom sf st_centroid st_point_on_surface st_nearest_feature st_intersection st_make_valid
#' @importFrom sf st_union st_as_sf st_drop_geometry st_intersects  st_relate
#' @importFrom dplyr bind_rows row_number distinct filter mutate summarize arrange
#' @importFrom dplyr filter select mutate all_of any_of left_join .data
#' @importFrom dplyr rename starts_with .data n desc bind_cols
#' @importFrom dplyr group_by ungroup slice pull left_join
#' @importFrom ggplot2 ggsave geom_sf labs theme_void scale_fill_brewer ggplot aes
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect
#' @importFrom readr read_csv write_csv
NULL
