#' Estimate Down Geography Levels
#'
#' Simple method for estimating data down to a lower level. This is most often useful
#' for getting election data down from a precinct level to a block level in the case
#' that a state or other jurisdiction split precincts when creating districts. Geographic
#' partner to estimate_down.
#'
#' @param from Larger geography level
#' @param to smaller geography level
#' @param wts numeric vector of length nrow(to). Defaults to 1. Typically population or VAP, as a weight to give each precinct.
#' @param value numeric vector of length nrow(from). Defaults to 1. Typically electoral outcomes, as a value to estimate down into blocks.
#' @param method string from center, centroid, point, or area for matching levels
#'
#' @return numeric vector with each value split by weight
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr group_by ungroup slice pull left_join
#' 
#' @export
#'
#' @examples \dontrun{
#' data("va18sub")
#' va18sub <- va18sub %>% filter(COUNTYFP == '087')
#' block <- create_block_table(state = 'VA', county = '087')  
#' disagg <- geo_estimate_down(from = va18sub, to = block, wts = block$VAP, value = va18sub$G18USSRSTE)
#' }
geo_estimate_down <- function(from, to, wts, value, method = 'center'){
  group <- geo_match(from = to, to = from, method = method)
  
  if(missing(wts)){
    wts <- 1
  }
  if(missing(value)){
    value <- 1
  }
  
  tb <- tibble(wts = wts, group = group) %>% 
    group_by(group) %>% 
    mutate(GTot = sum(wts)) %>% 
    ungroup() %>% 
    mutate(cont = wts/GTot)
  
  tb2 <- tibble(group = 1:length(value), value = value)
  
  tb <- tb %>% left_join(tb2, by = 'group') %>% mutate(out = cont*value)
  
  tb <- tb %>% mutate(out = ifelse(is.na(out), 0, out))
  
  return(tb$out)
}


#' Estimate Down Levels
#' 
#' Non-geographic partner function to geo_estimate_down. Allows users to estimate 
#' down without the costly matching operation if they've already matched.
#'
#' @param wts numeric vector. Defaults to 1. Typically population or VAP, as a weight to give each precinct.
#' @param value numeric vector. Defaults to 1. Typically electoral outcomes, as a value to estimate down into blocks.
#' @param group matches of length(wts) that correspond to row indices of value. Often, this input is the output of geo_match.
#' 
#' @return numeric vector with each value split by weight
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr group_by ungroup slice pull left_join
#' @export
#'
#' @examples \dontrun{
#' data("va18sub")
#' va18sub <- va18sub %>% filter(COUNTYFP == '087')
#' block <- create_block_table(state = 'VA', county = '087')  
#' groups <- geo_match(block, va18sub)
#' disagg <- estimate_down(wts = block$VAP, value = va18sub$G18USSRSTE, group = groups)
#' }
estimate_down <- function(wts, value, group){
  
  if(missing(wts)){
    wts <- 1
  }
  if(missing(value)){
    value <- 1
  }
  
  tb <- tibble(wts = wts, group = group) %>% 
    group_by(group) %>% 
    mutate(GTot = sum(wts)) %>% 
    ungroup() %>% 
    mutate(cont = wts/GTot)
  
  tb2 <- tibble(group = 1:max(group), value = value)
  
  tb <- tb %>% left_join(tb2, by = 'group') %>% mutate(out = cont*value)
  
  tb <- tb %>% mutate(out = ifelse(is.na(out), 0, out))
  
  return(tb$out)
}


#' Estimate Up Geography Levels
#'
#' Simple method for aggregating data up to a higher level This is most often useful
#' for getting population data from a block level up to a precinct level.
#' Geographic partner to estimate_up.
#'
#' @param from smaller geography level
#' @param to larger geography level
#' @param value numeric vector of length nrow(from). Defaults to 1. 
#' @param method string from center, centroid, point, or area for matching levels
#'
#' @return numeric vector with each value aggregated by group
#' 
#' @importFrom tibble tibble add_row
#' @importFrom dplyr group_by ungroup slice pull left_join
#' 
#' @export
#'
#' @examples \dontrun{
#' data("va18sub")
#' va18sub <- va18sub %>% filter(COUNTYFP == '087')
#' block <- create_block_table(state = 'VA', county = '087')  
#' agg <- geo_estimate_down(from = block, to = va18shp, value = block$Total)
#' }
geo_estimate_up <- function(from, to, value, method = 'center'){
  group <- geo_match(from = from, to = to, method = method)
  
  if(missing(value)){
    value <- 1
  }
  tb <- tibble(value = value, group = group) %>% 
    group_by(group) %>% 
    summarize(value = sum(value)) %>% 
    arrange(group)
  
  if(nrow(tb) < nrow(to)){
    for(i in 1:nrow(to)){
      if(tb$group[i] != i){
        tb <- tb %>% add_row(group = i, value = 0, .after = (i-1))
      }
        
    }
  }
  
  return(tb$value)
  
}

#' Estimate Up Levels
#' 
#' Non-geographic partner function to geo_estimate_up. Allows users to aggregate
#' up without the costly matching operation if they've already matched.
#'
#' @param value numeric vector. Defaults to 1. Typically population values.
#' @param group matches of length(value) that correspond to row indices of value. 
#' Often, this input is the output of geo_match.
#' 
#' @return numeric vector with each value aggregated by group
#' 
#' @importFrom tibble tibble add_row
#' @importFrom dplyr group_by ungroup slice pull left_join 
#' @export
#'
#' @examples \dontrun{
#' data("va18sub")
#' va18sub <- va18sub %>% filter(COUNTYFP == '087')
#' block <- create_block_table(state = 'VA', county = '087')  
#' groups <- geo_match(block, va18sub)
#' agg <- geo_estimate_down(alue = block$Total, group = groups)
#' }
estimate_up <- function(value, group){

  if(missing(value)){
    value <- 1
  }
  
  tb <- tibble(value = value, group = group) %>% 
    group_by(group) %>% 
    summarize(value = sum(value)) %>% 
    arrange(group)
  
  if(nrow(tb) < max(group)){
    for(i in 1:max(group)){
      if(tb$group[i] != i){
        tb <- tb %>% add_row(group = i, value = 0, .after = (i-1))
      }
      
    }
  }
  
  return(tb$value)
  
}


globalVariables(c('GTot', 'cont', 'out', 'group', 'value'))