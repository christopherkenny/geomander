#' Estimate Down Geography Levels
#'
#' Simple method for estimating data down to a lower level. This is most often useful
#' for getting election data down from a precinct level to a block level in the case
#' that a state or other jurisdiction split precincts when creating districts.
#'
#' @param from Larger geography level
#' @param to smaller geography level
#' @param wts numeric vector of length nrow(to). Defaults to 1. Typically population or VAP, as a weight to give each precinct.
#' @param value numeric vector of length nrow(from). Defaults to 1. Typically electoral outcomes, as a value to estimate down into blocks.
#' @param method string from center, centroid, point, or area for matching levels
#'
#' @return
#' @export
#'
#' @examples
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
    mutate(cont = wts/cont)
  
  tb2 <- tibble(group = 1:nrow(from), value = value)
  
  tb <- tb %>% left_join(tb2) %>% mutate(out = cont*value)
  
  tb <- tb %>% mtuate(out = ifelse(is.na(out), 0, out))
  
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
#' @return
#' @export
#'
#' @examples
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
    mutate(cont = wts/cont)
  
  tb2 <- tibble(group = 1:length(group), value = value)
  
  tb <- tb %>% left_join(tb2) %>% mutate(out = cont*value)
  
  tb <- tb %>% mtuate(out = ifelse(is.na(out), 0, out))
  
  return(tb$out)
}