#' Create Block Level Data
#'
#' Creates a block level dataset, using the decennial census information, with the
#' standard redistricting variables.
#'
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns blocks for the entire state.
#' @param geography Defaults to TRUE. Whether to return the geography or not.
#' @param year year, must be 2000, 2010, or 2020 once released.
#'
#' @return dataframe with data for each block in the selected region. Data includes
#' 2 sets of columns for each race or ethnicity category: population (pop) and
#' voting age population (vap)
#'
#' @importFrom tidycensus get_decennial
#' @importFrom tigris blocks
#' @importFrom dplyr filter select mutate all_of any_of left_join .data
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @export
#' @concept datatable
#' @examples
#' \dontrun{
#' # uses the Census API
#' create_block_table(state = 'NY', county = 'Rockland', geography = FALSE)
#' }
create_block_table <- function(state, county, geography = TRUE, year = 2010){

  if(! state %in% datasets::state.abb){
    stop('Please provide a two letter postal abbreviation for the state.')
  }
  statepo <- state

  if(year < 2000 || year %% 10 != 0){
    warning('Only 2010 and 2020 are currently supported. 2000 may or may not work.')
  }

  vars <- c(pop = 'P003001', pop_white = 'P005003', pop_black = 'P005004',
            pop_hisp = 'P004003', pop_aian = 'P005005', pop_asian = 'P005006',
            pop_nhpi = 'P005007', pop_other = 'P005008', pop_two = 'P005009',
            vap = 'P010001', vap_white = 'P011005', vap_black = 'P011006',
            vap_hisp = 'P011002', vap_aian = 'P011007', vap_asian = 'P011008',
            vap_nhpi = 'P011009', vap_other = 'P011010', vap_two = 'P011011',
            place = 'PLACE')


  if(year == 2000){

    vars_pop <- c(pop      = 'P004001', pop_white = 'P004005', pop_black = 'P004006',
                  pop_hisp = 'P004002', pop_aian  = 'P004007', pop_asian = 'P004008',
                  pop_nhpi = 'P004009', pop_other = 'P004010', pop_two   = 'P004011')

    vars_vap <- c(vap      = 'P006001', vap_white = 'P006005', vap_black = 'P006006',
                  vap_hisp = 'P006002', vap_aian  = 'P006007', vap_asian = 'P006008',
                  vap_nhpi = 'P006009', vap_other = 'P006010', vap_two   = 'P006011')


      if(missing(county)){
        out_pop <- get_decennial(geography = 'block', state = state, year = year,
                                 geometry = FALSE, keep_geo_vars = FALSE,
                                 variables = vars_pop)
        out_vap <- get_decennial(geography = 'block', state = state, year = year,
                                 geometry = FALSE, keep_geo_vars = FALSE,
                                 variables = vars_vap)
        out <- bind_rows(out_pop, out_vap)
      } else {
        out_pop <- get_decennial(geography = 'block', state = state, year = year,
                                 geometry = FALSE, keep_geo_vars = FALSE,
                                 variables = vars_pop)
        out_vap <- get_decennial(geography = 'block', state = state, year = year,
                                 geometry = FALSE, keep_geo_vars = FALSE,
                                 variables = vars_vap)
        out <- bind_rows(out_pop, out_vap)
      }
  } else {
    if(missing(county)){
      out <- get_decennial(geography = 'block', state = state, year = year,
                           geometry = FALSE, keep_geo_vars = FALSE,
                           variables = vars)
    } else {
      out <- get_decennial(geography = 'block', state = state, year = year,
                           geometry = FALSE, keep_geo_vars = FALSE, county = county,
                           variables = vars)
    }
  }



  out <- out %>% select(GEOID, variable, value) %>%
    pivot_wider(id_cols = GEOID, names_from = 'variable', values_from = 'value')

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
    names(blocks)[str_detect(names(blocks), pattern = 'STATE')] <- 'state'
    names(blocks)[str_detect(names(blocks), pattern = 'COUNTY')] <- 'county'
    names(blocks)[str_detect(names(blocks), pattern = 'TRACT')] <- 'tract'
    names(blocks)[str_detect(names(blocks), pattern = 'BLOCK')] <- 'block'

    out <- blocks %>% left_join(y = out, by = 'GEOID') %>% st_as_sf()

  }

  return(out)
}


#' Create Tract Level Data
#'
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns tracts for the entire state.
#' @param geography Defaults to TRUE. Whether to return the geography or not.
#' @param year year, must be >= 2009 and <= 2019.
#'
#' @return dataframe with data for each tract in the selected region. Data includes
#' 3 sets of columns for each race or ethnicity category: population (pop), voting age
#' population (vap), and citizen voting age population (cvap)
#' @export
#' @importFrom tidycensus get_acs
#' @importFrom dplyr rename starts_with .data
#' @concept datatable
#' @examples \dontrun{
#' # Relies on Census Bureau API
#' tract <- create_tract_table('NY', 'Rockland', year = 2018)
#' }
create_tract_table <- function(state, county, geography = TRUE, year = 2019){
  if(! state %in% datasets::state.abb){
    stop('Please provide a two letter postal abbreviation for the state.')
  }
  statepo <- state

  if(year < 2009 | year > 2019){
    warning('Only years in 2009:2019 inclusive are currently supported.')
  }

  fips <- tidycensus::fips_codes %>% filter(state == statepo)

  # totals + white + black + hisp / for total, vap, and cvap (by sex because acs...)
  vars <- c(pop         = 'B03002_001',
            pop_white   = 'B03002_003',
            pop_black   = 'B03002_004',
            pop_hisp    = 'B03002_012',
            pop_aian    = 'B03002_005',
            pop_asian   = 'B03002_006',
            pop_nhpi    = 'B03002_007',
            pop_other   = 'B03002_008',
            pop_two     = 'B03002_009',
            m_vap       = 'B05003_008',  m_nvap        = 'B05003_012',
            f_vap       = 'B05003_019',  f_nvap        = 'B05003_023',
            m_vap_black = 'B05003B_008', m_nvap_black  = 'B05003B_012',
            f_vap_black = 'B05003B_019', f_nvap_black  = 'B05003B_023',
            m_vap_white = 'B05003H_008', m_nvap_white  = 'B05003H_012',
            f_vap_white = 'B05003H_019', f_nvap_white  = 'B05003H_023',
            m_vap_hisp  = 'B05003I_008', m_nvap_hisp   = 'B05003I_012',
            f_vap_hisp  = 'B05003I_019', f_nvap_hisp   = 'B05003I_023',
            m_vap_aian  = 'B05003C_008', m_nvap_aian   = 'B05003C_012',
            f_vap_aian  = 'B05003C_019', f_nvap_aian   = 'B05003C_023',
            m_vap_asian = 'B05003D_008', m_nvap_asian  = 'B05003D_012',
            f_vap_asian = 'B05003D_019', f_nvap_asian  = 'B05003D_023',
            m_vap_nhpi  = 'B05003E_008', m_nvap_nhpi   = 'B05003E_012',
            f_vap_nhpi  = 'B05003E_019', f_nvap_nhpi   = 'B05003E_023',
            m_vap_other = 'B05003F_008', m_nvap_other  = 'B05003F_012',
            f_vap_other = 'B05003F_019', f_nvap_other  = 'B05003F_023',
            m_vap_two   = 'B05003G_008', m_nvap_two    = 'B05003G_012',
            f_vap_two   = 'B05003G_019', f_nvap_two    = 'B05003G_023'
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
    vap = .data$m_vap + .data$f_vap,
    vap_white = .data$m_vap_white + .data$f_vap_white,
    vap_black = .data$m_vap_black + .data$f_vap_black,
    vap_hisp  = .data$m_vap_hisp  + .data$f_vap_hisp,
    vap_aian  = .data$m_vap_aian  + .data$f_vap_aian,
    vap_asian = .data$m_vap_asian + .data$f_vap_asian,
    vap_nhpi  = .data$m_vap_nhpi  + .data$f_vap_nhpi,
    vap_other = .data$m_vap_other + .data$f_vap_other,
    vap_two   = .data$m_vap_two   + .data$f_vap_two
  ) %>% mutate(
    cvap = .data$vap - .data$m_nvap - .data$f_nvap,
    cvap_white = .data$vap_white - .data$m_nvap_white - .data$f_nvap_white,
    cvap_black = .data$vap_black - .data$m_nvap_black - .data$f_nvap_black,
    cvap_hisp  = .data$vap_hisp  - .data$m_nvap_hisp  - .data$f_nvap_hisp,
    cvap_aian  = .data$vap_aian  - .data$m_nvap_aian  - .data$f_nvap_aian,
    cvap_asian = .data$vap_asian - .data$m_nvap_asian - .data$f_nvap_asian,
    cvap_nhpi  = .data$vap_nhpi  - .data$m_nvap_nhpi  - .data$f_nvap_nhpi,
    cvap_other = .data$vap_other - .data$m_nvap_other - .data$f_nvap_other,
    cvap_two   = .data$vap_two   - .data$m_nvap_two   - .data$f_nvap_two
  ) %>% select(-starts_with(c('m','f'))
  )


  if(geography){
    if(!missing(county)){
      tract <- tigris::tracts(state = state, year = year, county = county)
    } else {
      tract <- tigris::tracts(state = state, year = year)
    }
    names(tract)[which(str_detect(names(tract), pattern = 'GEOID'))[1]] <- 'GEOID'
    names(tract)[which(str_detect(names(tract), pattern = 'STATE'))[1]] <- 'state'
    names(tract)[which(str_detect(names(tract), pattern = 'COUNTY'))[1]] <- 'county'
    names(tract)[which(str_detect(names(tract), pattern = 'TRACT'))[1]] <- 'tract'

    tract <- tract %>% select(.data$STATEFP, .data$COUNTYFP, .data$GEOID, .data$geometry)  %>%
      rename(State = STATEFP, County = COUNTYFP)

    out <- out %>% left_join(tract, by = 'GEOID') %>% st_as_sf()
  }

  return(out)
}

#' Create Block Group Level Data
#'
#' @param state Required. Two letter state postal code.
#' @param county Optional. Name of county.  If not provided, returns block groups for the entire state.
#' @param geography Defaults to TRUE. Whether to return the geography or not.
#' @param year year, must be >= 2009 and <= 2019.
#'
#' @return dataframe with data for each block group in the selected region. Data includes
#' 2 sets of columns for each race or ethnicity category: population (pop), voting age
#' population (vap)
#' @export
#' @importFrom tidycensus get_acs
#' @importFrom dplyr rename starts_with .data
#' @concept datatable
#' @examples \dontrun{
#' # Relies on Census Bureau API
#' bg <- create_block_group_table('NY', 'Rockland', year = 2018)
#' }
create_block_group_table <- function(state, county, geography = TRUE, year = 2019){
  if(! state %in% datasets::state.abb){
    stop('Please provide a two letter postal abbreviation for the state.')
  }
  statepo <- state


  fips <- tidycensus::fips_codes %>% filter(state == statepo)

  # totals + white + black + hisp / for total, vap, and cvap (by sex because acs...)
  vars <- c(pop         = 'B03002_001',
            pop_white   = 'B03002_003',
            pop_black   = 'B03002_004',
            pop_hisp    = 'B03002_012',
            pop_aian    = 'B03002_005',
            pop_asian   = 'B03002_006',
            pop_nhpi    = 'B03002_007',
            pop_other   = 'B03002_008',
            pop_two     = 'B03002_009',
            m_vap       = 'B05003_008',
            f_vap       = 'B05003_019',
            m_vap_black = 'B05003B_008',
            f_vap_black = 'B05003B_019',
            m_vap_white = 'B05003H_008',
            f_vap_white = 'B05003H_019',
            m_vap_hisp  = 'B05003I_008',
            f_vap_hisp  = 'B05003I_019',
            m_vap_aian  = 'B05003C_008',
            f_vap_aian  = 'B05003C_019',
            m_vap_asian = 'B05003D_008',
            f_vap_asian = 'B05003D_019',
            m_vap_nhpi  = 'B05003E_008',
            f_vap_nhpi  = 'B05003E_019',
            m_vap_other = 'B05003F_008',
            f_vap_other = 'B05003F_019',
            m_vap_two   = 'B05003G_008',
            f_vap_two   = 'B05003G_019'
  )


  if(missing(county)){
    out <- tibble()

    for(cty in fips$county){
      block <- tidycensus::get_acs(geography = 'block group', state = state, year = year,
                                   geometry = FALSE, keep_geo_vars = FALSE, county = cty,
                                   variables = vars)
      out <- rbind(out, block)
    }

  } else {
    out <- tidycensus::get_acs(geography = 'block group', state = state, year = year,
                               geometry = FALSE, keep_geo_vars = FALSE, county = county,
                               variables = vars)
  }

  out <- out %>% select(GEOID, variable, estimate) %>%
    pivot_wider(id_cols = GEOID, names_from = 'variable', values_from = 'estimate')

  out <- out %>% mutate(
    vap = .data$m_vap + .data$f_vap,
    vap_white = .data$m_vap_white + .data$f_vap_white,
    vap_black = .data$m_vap_black + .data$f_vap_black,
    vap_hisp  = .data$m_vap_hisp  + .data$f_vap_hisp,
    vap_aian  = .data$m_vap_aian  + .data$f_vap_aian,
    vap_asian = .data$m_vap_asian + .data$f_vap_asian,
    vap_nhpi  = .data$m_vap_nhpi  + .data$f_vap_nhpi,
    vap_other = .data$m_vap_other + .data$f_vap_other,
    vap_two   = .data$m_vap_two   + .data$f_vap_two
  ) %>% select(-starts_with(c('m','f'))
  )


  if(geography){
    if(!missing(county)){
      bgs <- tigris::block_groups(state = state, year = year, county = county)
    } else {
      bgs <- tigris::block_groups(state = state, year = year)
    }

    bgs <- bgs %>% select(.data$STATEFP, .data$COUNTYFP, .data$GEOID, .data$geometry) %>%
      rename(State = STATEFP, County = COUNTYFP)

    out <- out %>% left_join(bgs, by = 'GEOID') %>% st_as_sf()
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
#' @concept datatable
#' @examples
#' set.seed(1)
#' data(rockland)
#' rockland$id <- sample(1:2, nrow(rockland), TRUE)
#' block2prec(rockland, rockland$id)
#'
block2prec <- function(block_table, matches, geometry = FALSE){
  if(missing(block_table)){
    stop("Please provide an argument to block_table.")
  }
  if(missing(matches)){
    stop("Please provide an argument to matches")
  }

  block_table <- block_table %>% mutate(matches_id = matches)

  if(!geometry){
    ret <- block_table %>% sf::st_drop_geometry() %>%
      group_by(matches_id) %>%
      dplyr::summarize(
        state = ifelse(length(unique(.data$state)) == 1, unique(.data$state), NA),
        county = ifelse(length(unique(.data$county)) == 1, unique(.data$county), NA),
        pop       = sum(.data$pop),
        pop_white = sum(.data$pop_white),
        pop_black = sum(.data$pop_black),
        pop_hisp  = sum(.data$pop_hisp),
        pop_aian  = sum(.data$pop_aian),
        pop_asian = sum(.data$pop_asian),
        pop_nhpi  = sum(.data$pop_nhpi),
        pop_other = sum(.data$pop_other),
        pop_two   = sum(.data$pop_two),
        vap       = sum(.data$vap),
        vap_hisp  = sum(.data$vap_hisp),
        vap_white = sum(.data$vap_white),
        vap_black = sum(.data$vap_black),
        vap_aian  = sum(.data$vap_aian),
        vap_asian = sum(.data$vap_asian),
        vap_nhpi  = sum(.data$vap_nhpi),
        vap_other = sum(.data$vap_other),
        vap_two   = sum(.data$vap_two),
        .groups = 'drop'
      )
  } else {
    ret <- block_table %>%
      group_by(matches_id) %>%
      dplyr::summarize(
        state = ifelse(length(unique(.data$state)) == 1, unique(.data$state), NA),
        county = ifelse(length(unique(.data$county)) == 1, unique(.data$county), NA),
        pop       = sum(.data$pop),
        pop_white = sum(.data$pop_white),
        pop_black = sum(.data$pop_black),
        pop_hisp  = sum(.data$pop_hisp),
        pop_aian  = sum(.data$pop_aian),
        pop_asian = sum(.data$pop_asian),
        pop_nhpi  = sum(.data$pop_nhpi),
        pop_other = sum(.data$pop_other),
        pop_two   = sum(.data$pop_two),
        vap       = sum(.data$vap),
        vap_hisp  = sum(.data$vap_hisp),
        vap_white = sum(.data$vap_white),
        vap_black = sum(.data$vap_black),
        vap_aian  = sum(.data$vap_aian),
        vap_asian = sum(.data$vap_asian),
        vap_nhpi  = sum(.data$vap_nhpi),
        vap_other = sum(.data$vap_other),
        vap_two   = sum(.data$vap_two),
        geometry = st_union(geometry),
        .groups = 'drop'
      ) %>% st_as_sf()
  }

  ret <- ret %>% arrange(matches_id)

  if(nrow(ret) != max(matches)){
    for(i in 1:max(matches)){
      if(ret$matches_id[i] != i){
        ret <- ret %>% add_row(matches_id = i,
                               state = ifelse(length(unique(.data$state)) == 1, unique(.data$state), NA),
                               county = ifelse(length(unique(.data$county)) == 1, unique(.data$county), NA),
                               pop       = 0,
                               pop_white = 0,
                               pop_black = 0,
                               pop_hisp  = 0,
                               pop_aian  = 0,
                               pop_asian = 0,
                               pop_nhpi  = 0,
                               pop_other = 0,
                               pop_two   = 0,
                               vap       = 0,
                               vap_hisp  = 0,
                               vap_white = 0,
                               vap_black = 0,
                               vap_aian  = 0,
                               vap_asian = 0,
                               vap_nhpi  = 0,
                               vap_other = 0,
                               vap_two   = 0,
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
#' @concept datatable
#' @examples \dontrun{
#' # Need Census API
#' data(towns)
#' towns$fips <- '087'
#' block <- create_block_table('NY', 'Rockland')
#' block2prec_by_county(block, towns, 'fips')
#' }
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


  precinct <- precinct %>% mutate(rowid = row_number()) %>%
    select(rowid, geometry, all_of(precinct_county_fips))

  prectb <- tibble()
  countiesl <- unique(block_table$county)

  for(cty in 1:length(countiesl)){

    bsub <- block_table %>% filter(.data$county == countiesl[cty])
    psub <- precinct %>% filter(.data[[precinct_county_fips]] == countiesl[cty]) %>%
      mutate(matches_id = row_number())

    matches <- geo_match(from = bsub, to = psub)
    prectemp <- block2prec(bsub, matches = matches)

    prectemp <- prectemp %>% left_join(y = psub %>% sf::st_drop_geometry()
                                       %>% select(rowid, matches_id), by = 'matches_id')

    prectb <- prectb %>% bind_rows(prectemp)
  }

  prectb <- prectb %>% arrange(rowid) %>% mutate(matches_id = rowid) %>% select(-rowid)

  return(prectb)
}


globalVariables(c('GEOID', 'variable', 'value', 'AWATER10', 'ALAND10', 'County',
                  'State', 'pop', 'pop_black', 'pop_hisp', 'pop_other',
                  'pop_white', 'vap', 'rowid', 'geometry', '.data',
                  'COUNTYFP', 'cvap_black', 'cvap_hisp', 'cvap_white', 'STATEFP',
                  'estimate',
                  'vap_black', 'vap_hisp', 'vap_other', 'vap_white', 'matches_id',
                  "m_vap", "m_nvap", "f_vap", "f_nvap", "m_vap_black", "Mnvap_black",
                  "f_vap_black",  "f_nvap_black", "m_vap_white",  "Mnvap_white", "f_vap_white",
                  "f_nvap_white", "m_vap_hisp",   "Mnvap_hisp", "f_vap_hisp",   "f_nvap_hisp"))
