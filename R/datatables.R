#' Create Block Level Data
#' 
#' Creates a block level dataset, using the decennial census information, with the 
#' standard redistricting variables.
#'
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns blocks for the entire state.
#' @param geography Defaults to TRUE. Whether to return the geography or not.
#' @param year year, must be 2010 at the moment. 2020 to be added once available. 2000 if rereleased.
#'
#' @return dataframe with data for each block in the selected region
#' 
#' @importFrom tidycensus get_decennial
#' @importFrom tigris blocks
#' @importFrom dplyr filter select mutate all_of any_of left_join
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @export
#' 
#' @examples \dontrun{
#' create_block_table(state = 'NY', county = 'Rockland', geography = F)
#' }
create_block_table <- function(state, county, geography = TRUE, year = 2010){
  
  if(! state %in% datasets::state.abb){
    stop('Please provide a two letter postal abbreviation for the state.')
  }
  statepo <- state
  
  if(year != 2010){
    stop('Only 2010 is currently supported due to SF3 2000 issues and delay of 2020 data.')
  }
  
  fips <- tidycensus::fips_codes %>% filter(state == statepo)
  
  vars <- c('P003001', 'P005003', 'P005004', 'P004003', 'P010001', 'P011005','P011006', 'P011002', 'PLACE')
  
  
  if(missing(county)){
    out <- tibble()
    
    for(cty in fips$county){
      block <- get_decennial(geography = 'block', state = state, year = year, geometry = FALSE, keep_geo_vars = FALSE, county = cty,
                             variables = vars)
      out <- rbind(out, block)
    }
  } else {
    out <- get_decennial(geography = 'block', state = state, year = year, geometry = FALSE, keep_geo_vars = FALSE, county = county,
                         variables = vars)
  }
  
  out <- out %>% select(GEOID, variable, value) %>% pivot_wider(id_cols = GEOID, names_from = 'variable', values_from = 'value')
  
  if(geography){
    if(!missing(county)){
      blocks <- tigris::blocks(state = state, year = year, county = county) 
    } else {
      blocks <- tigris::blocks(state = state, year = year) 
    }
    
    blocks <- blocks %>% mutate(waterpct = AWATER10/(ALAND10+AWATER10))
    blocks <- blocks %>% select(any_of(c('STATEFP10', 'COUNTYFP10', 'TRACTCE10', 'BLOCKCE10', 'GEOID10',
                                       'STATEFP00', 'COUNTYFP00', 'TRACTCE00', 'BLOCKCE00', 'GEOID00',
                                       'waterpct', 'geometry')))
    names(blocks)[str_detect(names(blocks), pattern = 'GEOID')] <- 'GEOID'

    out <- blocks %>% left_join(y = out)
    
    names(out) <- c('State', 'County', 'Tract', 'Block', 'GEOID', 'waterpct', 
                     'Total', 'TotalWhite', 'TotalBlack', 'TotalHisp', 'VAP', 'VAPWhite', 'VAPBlack', 'VAPHisp', 'Place', 'geometry')
    
  } else {
    names(out) <- c('GEOID', 'Total', 'TotalWhite', 'TotalBlack', 'TotalHisp', 'VAP', 'VAPWhite', 'VAPBlack', 'VAPHisp', 'Place')
  }
  
  out <- out %>% mutate(TotalOther = Total - TotalWhite - TotalBlack - TotalHisp,
                        VAPOther = VAP - VAPWhite - VAPBlack - VAPHisp)
  
  return(out)
}

globalVariables(c('GEOID', 'variable', 'value', 'AWATER10', 'ALAND10'))