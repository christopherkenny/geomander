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


#' Create Tract Level Data
#'
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns tracts for the entire state.
#' @param geography Defaults to TRUE. Whether to return the geography or not.
#' @param year year, must be >= 2009 and <= 2019.
#'
#' @return dataframe with data for each tract in the selected region
#' @export
#' @importFrom tidycensus get_acs
#' @importFrom dplyr rename starts_with
#' @examples \dontrun{
#' tract <- create_tract_table('NY', 'Rockland', year = 2018)
#' }
create_tract_table <- function(state, county, geography = TRUE, year = 2019){
  if(! state %in% datasets::state.abb){
    stop('Please provide a two letter postal abbreviation for the state.')
  }
  statepo <- state
  
  if(year < 2009 | year > 2019){
    stop('Only years in 2009:2019 inclusive are currently supported.')
  }
  
  fips <- tidycensus::fips_codes %>% filter(state == statepo)
  
  # totals + white + black + hisp / for total, vap, and cvap (by sex because acs...)
  vars <- c(Total = 'B03002_001',  TotalWhite = 'B03002_003',  
            TotalBlack = 'B03002_004', TotalHisp = 'B03002_012',
            MVAP = 'B05003_008', MNVAP = 'B05003_012', 
            FVAP = 'B05003_019',    FNVAP = 'B05003_023',
            MVAPBlack = 'B05003B_008',  MNVAPBlack = 'B05003B_012',
            FVAPBlack = 'B05003B_019', FNVAPBlack = 'B05003B_023',
            MVAPWhite = 'B05003H_008', MNVAPWhite  = 'B05003H_012',
            FVAPWhite  = 'B05003H_019', FNVAPWhite  = 'B05003H_023',
            MVAPHisp = 'B05003I_008', MNVAPHisp = 'B05003I_012',
            FVAPHisp = 'B05003I_019', FNVAPHisp = 'B05003I_023'
            )
  
  
  if(missing(county)){
    out <- tibble()
    
    for(cty in fips$county){
      block <- get_acs(geography = 'tract', state = state, year = year, 
                             geometry = FALSE, keep_geo_vars = FALSE, county = cty,
                             variables = vars)
      out <- rbind(out, block)
    }
    
  } else {
    out <- get_acs(geography = 'tract', state = state, year = year, 
                         geometry = FALSE, keep_geo_vars = FALSE, county = county, 
                         variables = vars)
  }
  
  out <- out %>% select(GEOID, variable, estimate) %>% 
    pivot_wider(id_cols = GEOID, names_from = 'variable', values_from = 'estimate')

  out <- out %>% mutate(
    TotalOther = Total - TotalWhite - TotalBlack - TotalHisp,
    VAP = MVAP + FVAP,
    VAPWhite = MVAPWhite + FVAPWhite,
    VAPBlack = MVAPBlack + FVAPBlack,
    VAPHisp = MVAPHisp + FVAPHisp
  ) %>% mutate(
    VAPOther = VAP - (VAPWhite + VAPBlack + VAPHisp)
  ) %>% mutate(
    CVAP = VAP - MNVAP - FNVAP,
    CVAPWhite = VAPWhite - MNVAPWhite - FNVAPWhite,
    CVAPBlack = VAPBlack - MNVAPBlack - FNVAPBlack,
    CVAPHisp = VAPHisp - MNVAPHisp - FNVAPHisp,
  ) %>% mutate(
    CVAPOther = VAP - (CVAPWhite + CVAPBlack + CVAPHisp)
  ) %>% select(-starts_with(c('M','F'))
  )
  
  
  if(geography){
    if(!missing(county)){
      tracts <- tigris::tracts(state = state, year = year, county = county) 
    } else {
      tracts <- tigris::tracts(state = state, year = year) 
    }
    
    tracts <- tracts %>% select(STATEFP, COUNTYFP, GEOID, geometry) %>% 
      rename(State = STATEFP, County = COUNTYFP)
    
    out <- out %>% left_join(tracts, by = 'GEOID')
  }
  
  return(out)
}


#'  Aggregate Block Table by Matches
#'
#' Aggregates block table values up to a higher level, normally precincts, hence
#' the name block2prec. 
#'
#' @param block_table Required. Block table output from create_block_table
#' @param matches Required. Grouping variable to aggregate up by, typically made with geo_match
#' @param geometry Boolean. Whether to keep geometry or not.
#'
#' @importFrom sf st_union st_as_sf st_drop_geometry
#' @importFrom dplyr summarize arrange 
#' @return dataframe with length(unique(matches)) rows
#' @export
block2prec <- function(block_table, matches, geometry = FALSE){
  if(missing(block_table)){
    stop("Please provide an argument to block_table.")
  }
  if(missing(matches)){
    stop("Please provide an argument to matches")
  }
  
  block_table <- block_table %>% mutate(matches_id = matches)
  
  if(!geometry){
    ret <- block_table %>% st_drop_geometry() %>% 
      group_by(matches_id) %>% summarize(
        State = unique(State),
        County = unique(County),
        Total = sum(Total),
        TotalWhite = sum(TotalWhite),
        TotalBlack = sum(TotalBlack),
        TotalHisp = sum(TotalHisp),
        TotalOther = sum(TotalOther),
        VAP = sum(VAP),
        VAPWhite = sum(VAPWhite),
        VAPBlack = sum(VAPBlack),
        VAPHisp = sum(VAPHisp),
        VAPOther = sum(VAPOther), .groups = 'drop'
      )
  } else {
    ret <- block_table %>% 
      group_by(matches_id) %>% summarize(
        State = unique(State),
        County = unique(County),
        Total = sum(Total),
        TotalWhite = sum(TotalWhite),
        TotalBlack = sum(TotalBlack),
        TotalHisp = sum(TotalHisp),
        TotalOther = sum(TotalOther),
        VAP = sum(VAP),
        VAPWhite = sum(VAPWhite),
        VAPBlack = sum(VAPBlack),
        VAPHisp = sum(VAPHisp),
        VAPOther = sum(VAPOther),
        geometry = st_union(geometry), .groups = 'drop'
      ) %>% st_as_sf()
  }
  
  ret <- ret %>% arrange(matches_id)
  
  if(nrow(ret) != max(matches)){
    for(i in 1:max(matches)){
      if(ret$matches_id[i] != i){
        ret <- ret %>% add_row(matches_id = i, 
                               State = ifelse(length(unique(ret$State)) == 1, unique(ret$State), NA),
                               County = ifelse(length(unique(ret$County)) == 1, unique(ret$County), NA),
                               Total = 0,
                               TotalWhite = 0,
                               TotalBlack = 0,
                               TotalHisp = 0,
                               TotalOther = 0,
                               VAP = 0,
                               VAPWhite = 0,
                               VAPBlack = 0,
                               VAPHisp = 0,
                               VAPOther = 0, 
                               .after = (i-1))
      }

    }
  }
  
  
  return(ret)
}







#' Aggregate Block Table by Matches and County
#' 
#' Performs the same type of operation as block2prec, but subsets a precinct geometry
#' based on a County fips column. This helps get around the problem that county geometries
#' often have borders that follow rivers and lead to funny shaped blocks. This guarantees
#' that every block is matched to a precinct which is in the same county.
#'
#' @param block_table Required. Block table output from create_block_table
#' @param precinct sf dataframe of shapefiles to match to.
#' @param precinct_county_fips Column within precincts 
#' 
#' @return dataframe with nrow(precinct) rows
#' @export
block2prec_by_county <- function(block_table, precinct, precinct_county_fips){
  
  if(missing(block_table)){
    stop('Please provide an argument to block_table.')
  }
  if(missing(precinct)){
    stop('Please provide an argument to precincts.')
  }
  if(missing(precinct_county_fips)){
    stop('Please provide an argument to precinct_county_fips.')
  }
  if(!precinct_county_fips %in% names(precinct)){
    stop('precinct_county_fips is not the name of a column in precinct.')
  }
  
  
  precinct <- precinct %>% mutate(rowid = row_number()) %>% select(rowid, geometry, all_of(precinct_county_fips))
  
  prectb <- tibble()
  counties <- unique(block_table$County)
  
  for(cty in 1:length(counties)){

    bsub <- blocks %>% filter(County == counties[cty])
    psub <- precinct %>% filter(.data[[precinct_county_fips]] == counties[cty]) %>% 
      mutate(matches_id = row_number())
    
    matches <- geo_match(from = bsub, to = psub)
    prectemp <- block2prec(bsub, matches = matches)
    
    prectemp <- prectemp %>% left_join(y = psub %>% st_drop_geometry() %>% select(rowid, matches_id), by = 'matches_id')
    
    prectb <- prectb %>% bind_rows(prectemp)
  }
  
  prectb <- prectb %>% arrange(rowid)
  
  return(prectb)
}


globalVariables(c('GEOID', 'variable', 'value', 'AWATER10', 'ALAND10', 'County',
                  'State', 'Total', 'TotalBlack', 'TotalHisp', 'TotalOther', 
                  'TotalWhite', 'VAP', 'rowid', 'geometry', '.data',
                  'COUNTYFP', 'CVAPBlack', 'CVAPHisp', 'CVAPWhite', 'STATEFP', 
                  'estimate', 
                  'VAPBlack', 'VAPHisp', 'VAPOther', 'VAPWhite', 'matches_id',
                  "MVAP", "MNVAP", "FVAP", "FNVAP", "MVAPBlack", "MNVAPBlack",
                  "FVAPBlack",  "FNVAPBlack", "MVAPWhite",  "MNVAPWhite", "FVAPWhite",
                  "FNVAPWhite", "MVAPHisp",   "MNVAPHisp", "FVAPHisp",   "FNVAPHisp"))