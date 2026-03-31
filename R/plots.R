#' Plot Groups with Connected Components Colored
#'
#' @param shp An `sf` object.
#' @param adj Adjacency list.
#' @param group Optional vector of group identifiers, typically district numbers
#'   or county names.
#' @param save Logical. If `TRUE`, save each plot to disk.
#' @param path Directory prefix used when `save = TRUE`.
#'
#' @return list of `ggplot` objects, one per unique group
#' @export
#'
#' @concept plot
#'
#' @examples
#' library(dplyr)
#' data('checkerboard')
#' data('checkerboard_adj')
#'
#' checkerboard <- checkerboard |> mutate(discont = as.integer(j == 5 | j == 6))
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

  shp <- shp |>
    dplyr::bind_cols(components) |>
    dplyr::mutate(rownum = row_number())


  out <- list()
  for (g in 1:length(unique(group))) {
    gc <- unique(group)[g]

    temp <- shp |>
      dplyr::filter(group == gc)

    p <- temp |>
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

#' Plot an `sf` Object with Row Numbers
#'
#' Quick diagnostic plot that labels each row with its row number.
#'
#' @param shp An `sf` object.
#'
#' @concept plot
#'
#' @return `ggplot` object
#' @export
#'
#' @examples
#' data(checkerboard)
#' geo_plot(checkerboard)
#'
geo_plot <- function(shp) {
  shp |>
    dplyr::mutate(rn = dplyr::row_number()) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf() +
    ggplot2::geom_sf_text(aes(label = .data$rn)) +
    ggplot2::theme_void()
}

globalVariables('component')
