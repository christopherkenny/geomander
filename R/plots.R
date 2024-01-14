#' Create Plots of Shapes by Group with Connected Components Colored
#'
#' @param shp An sf shapefile
#' @param adj adjacency list
#' @param group array of group identifiers. Typically district numbers or county names.
#' @param save Boolean, whether to save or not.
#' @param path Path to save, only used if save is TRUE. Defaults to working directory.
#'
#' @return list of ggplots
#' @export
#'
#' @concept plot
#'
#' @examples
#' library(dplyr)
#' data('checkerboard')
#' data('checkerboard_adj')
#'
#' checkerboard <- checkerboard %>% mutate(discont = as.integer(j == 5 | j == 6))
#'
#' p <- geo_plot_group(checkerboard, checkerboard_adj, checkerboard$discont)
#'
#' p[[1]]
#' p[[2]]
geo_plot_group <- function(shp, adj, group, save = FALSE, path = '') {
  if (missing(shp)) {
    cli::cli_abort('Please provide an argument to {.arg shp}.')
  }

  if (missing(adj)) {
    cli::cli_abort('Please provide an argument to {.arg adj}.')
  }

  if (missing(group)) {
    group <- rep(1L, nrow(shp))
  }

  components <- check_contiguity(adj = adj, group = group)

  shp <- shp %>%
    dplyr::bind_cols(components) %>%
    dplyr::mutate(rownum = row_number())


  out <- list()
  for (g in 1:length(unique(group))) {
    gc <- unique(group)[g]

    temp <- shp %>%
      dplyr::filter(group == gc)

    p <- temp %>%
      ggplot2::ggplot(aes(fill = as.character(component))) +
      ggplot2::geom_sf() +
      ggplot2::labs(fill = 'Conn Comp', title = gc) +
      ggplot2::theme_void() +
      ggplot2::scale_fill_brewer(type = 'qual', palette = 'Dark2')

    out[[g]] <- p
    if (save) {
      ggsave(filename = paste0(path, 'group_plot_', gc, '.png'), plot = p)
    }
  }

  out
}

#' Plots a Shape with Row Numbers as Text
#'
#' One liner to plot a shape with row numbers
#'
#' @param shp An sf shapefile
#'
#' @concept plot
#'
#' @return ggplot
#' @export
#'
#' @examples
#' data(checkerboard)
#' geo_plot(checkerboard)
#'
geo_plot <- function(shp) {
  shp %>%
    dplyr::mutate(rn = dplyr::row_number()) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf() +
    ggplot2::geom_sf_text(aes(label = .data$rn)) +
    ggplot2::theme_void()
}

globalVariables('component')
