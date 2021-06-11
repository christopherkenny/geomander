#' Split a Precinct 
#'
#' States often split a precinct when they create districts but rarely provide the 
#' geography for the split precinct. This allows you to split a precinct using a
#' lower geography, typically blocks.
#'
#' @param lower The lower geography that makes up the precinct, this is often a 
#' block level geography.
#' @param precinct The single precinct that you would like to split.
#' @param split_by The upper geography that you want to split precinct by
#' @param lower_wt Optional. Numeric weights to give to each precinct, typically 
#' VAP or population.
#' @param split_by_id Optional. A string that names a column in split_by that 
#' identifies each observation in split_by
#'
#' @return sf data frame with precinct split
#' @export
#'
#' @concept fix
#'
#' @examples
#' library(dplyr)
#' library(sf)
#' data(checkerboard)
#' low <- checkerboard %>% slice(1:3, 9:11)
#' prec <- checkerboard %>% slice(1:3) %>% summarize(geometry = st_union(geometry)) 
#' dists <-  checkerboard %>% slice(1:3, 9:11) %>% mutate(dist = c(1,2,2,1,3,3)) %>% 
#' group_by(dist) %>% summarize(geometry = st_union(geometry))
#' 
#' split_precinct(low, prec, dists, split_by_id = 'dist')
#' 
split_precinct <- function(lower, precinct, split_by, lower_wt, split_by_id){
  
  if(!missing(lower_wt)){
    if(length(lower_wt) != nrow(lower)){
      stop('`lower_wt` must have the same number of entries as `lower`')
    }
    
    if(!is.numeric(lower_wt)){
      stop('`lower_wt` must be a numeric vector to allow for weighting.')
    }
  }
  
  if(nrow(precinct) != 1){
    stop('Please provide only one geography to `precinct`.')
  }
  
  if(nrow(split_by) < 2){
    stop('`split_by` requires at least two geographies to consider.')
  }
  
  lower <- lower %>% geo_filter(precinct) %>% geo_trim(precinct)
  
  split_by <- split_by %>% geo_filter(precinct)
  
  matches <- geo_match(from = lower, to = split_by)
  
  out_geo <- lower %>% select(.data$geometry) %>% mutate(new = matches) %>% 
    group_by(.data$new) %>% summarize(geometry = st_union(.data$geometry))
  
  
  if(!missing(lower_wt)){
   out_wt <- tibble(new = matches, wt = lower_wt) %>% 
     group_by(.data$new) %>% 
     summarize(wt = sum(.data$lower_wt, na.rm = TRUE), .groups = 'drop')
   
   out_geo <- left_join(out_geo, out_wt, by = c('new'))
  }
  
  if(!missing(split_by_id)){
    if(split_by_id %in% names(split_by)){
      out_geo$id <- split_by[[split_by_id]][out_geo$new]
    } else {
      warning('`split_by_id` provided, but no column with that name found.')
    }
  }
  
  return(out_geo)
}

