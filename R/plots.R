#' Create Plots of Shapes by Group with Connected Components Colored
#'
#' @param shp An sf shapefile
#' @param adjacency adjacency list
#' @param group array of group identifiers. Typically district numbers or county names.
#' @param save Boolean, whether to save or not.
#' @param path Path to save, only used if save is TRUE. Defaults to working directory.
#'
#' @return list of ggplots
#' @export
#' @importFrom ggplot2 ggsave geom_sf labs theme_void scale_fill_brewer ggplot aes
#' @importFrom dplyr bind_cols
#' 
#' @concept plot
#' 
#' @examples
#' library(dplyr)
#' data("checkerboard")
#' data("checkerboard_adj")
#' 
#' checkerboard <- checkerboard %>% mutate(discont = as.integer(j == 5| j == 6))
#' 
#' p <- geo_plot_group(checkerboard, checkerboard_adj, checkerboard$discont)
#' 
#' p[[1]]
#' p[[2]]
#' 
geo_plot_group <- function(shp, adjacency, group, save = F, path = ''){
  if(missing(shp)){
    stop('Please provide an argument to shp.')
  }
  
  if(missing(adjacency)){
    stop('Please provide an argument to adjacency.')
  }
  
  if(missing(group)){
    group <- rep(1L, nrow(shp))
  }
  
  components <- check_contiguity(adjacency = adjacency, group = group)
  
  shp <- shp %>% bind_cols(components) %>% mutate(rownum = row_number())
  
  
  out <- list()
  for(g in 1:length(unique(group))){
    gc <- unique(group)[g]
    
    temp <- shp %>% filter(group == gc)
    
    p <- temp %>% ggplot(aes(fill = as.character(component))) +
      geom_sf() +
      labs(fill = 'Conn Comp', title = gc) +
      theme_void() +
      scale_fill_brewer(type = 'qual', palette = 'Dark2')
    
    out[[g]] <- p
    if(save){
      p + ggsave(filename = paste0(path, 'group_plot_', gc, '.png'))
    }
  }
  
  return(out)
  
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
#' @importFrom ggplot2 geom_sf_text
#' @importFrom dplyr row_number
#' 
#' 
#' @examples 
#' data(checkerboard)
#' geo_plot(checkerboard)
#' 
geo_plot <- function(shp){
  shp %>% 
    mutate(rn = row_number()) %>% 
    ggplot() + 
    geom_sf() + 
    geom_sf_text(aes(label = .data$rn)) + 
    theme_void()
}

globalVariables('component')