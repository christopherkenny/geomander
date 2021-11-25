#' DRA to R
#'
#' Creates a block or precinct level dataset from DRA csv output.
#'
#' @param dra The path to an exported csv or
#' a dataframe with columns Id and District, loaded from a DRA export.
#' @param state the state postal code of the state
#' @param precincts an sf dataframe of precinct shapes to link the output to
#'
#' @return sf dataframe either at the block level or precinct level
#' @export
#' @concept dra
#' @examples\dontrun{
#' # Needs Census Bureau API
#' # dra_utah_test is available at https://bit.ly/3c6UDKk
#' blocklevel <- dra2r('dra_utah_test.csv', state = 'UT')
#' }
dra2r <- function(dra, state, precincts) {

  # check inputs
  if (missing(dra)) {
    stop('Please provide an argument to `dra`.')
  }
  if (missing(state)) {
    stop('Please provide a postal code, fips code, or name to state.')
  }

  # make dra into a dataframe
  if ('character' %in% class(dra)) {

    # check that it's pointing to a good thing:
    if (!file.exists(dra)) {
      stop('dra supplied as character, but does not point to a valid file.')
    }

    # get the file extension
    ext <- stringr::str_sub(dra, start = -4)
    if (ext == '.csv') {
      dra <- readr::read_csv(file = dra, col_types = c(Id = 'c', District = 'c'), col_names = TRUE)
    } else if (ext == 'json') {
      stop('Please export the map as a csv. json not currently supported, but may be in the future.')
    } else {
      stop('dra points to a valid file, but not a csv. Please point to a csv export of the map.')
    }


    if (!('Id' %in% names(dra) & 'District' %in% names(dra))) {
      stop('dra points to a file where `Id` and/or `District` column not present.')
    }
  } else {
    if (!('Id' %in% names(dra) & 'District' %in% names(dra))) {
      stop('dra provided as dataframe, but `Id` and/or `District` column not present.')
    }
  }

  # rename District column
  dra <- dra %>% rename(District_DRA = District)

  # get the block file to match it to
  shp <- tigris::blocks(state)
  shp$GEOID10 <- as.double(shp$GEOID10)

  # join them together
  shp <- shp %>% left_join(dra, by = c('GEOID10' = 'Id'))

  # match to precincts if provided
  if (!missing(precincts)) {
    if (sf::st_crs(shp) != sf::st_crs(precincts)) {
      shp <- sf::st_transform(shp, sf::st_crs(precincts))
    }

    precincts <- precincts %>% mutate(matches = row_number())

    matches <- geo_match(from = shp, to = precincts)

    shp <- shp %>% mutate(matches = matches)

    shp <- shp %>%
      sf::st_drop_geometry() %>%
      select(matches, District_DRA)

    # get the most likely match
    shp <- shp %>%
      group_by(matches, District_DRA) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      group_by(matches) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      ungroup() %>%
      select(-n)

    precincts <- precincts %>%
      left_join(shp, by = 'matches') %>%
      select(-matches)

    return(precincts)
  }

  shp
}






#' R to DRA
#'
#' Project a plan at the precinct level down to blocks into a format that can be used
#' with DRA. Projecting down to blocks can take a lot of time for larger states.
#'
#' @param precincts Required. an sf dataframe of precinct shapes
#' @param plan Required. Either a vector of district assignments
#' or the name of a column in precincts with district assignments.
#' @param state Required. the state postal code of the state
#' @param path Optional. A path to try to save to. Warns if saving failed.
#'
#' @return tibble with columns Id, as used by DRA, identical to GEOID in census terms and District.
#' @export
#' @concept dra
#' @examples \dontrun{
#' # Needs Census Bureau API
#' cd <- tigris::congressional_districts() %>% filter(STATEFP == '49')
#' cnty <- tigris::counties(state = 49)
#' matchedcty <- geo_match(from = cnty, to = cd)
#' # use counties as precincts and let the plan be their center match:
#' r2dra(cnty, matchedcty, 'UT', 'r2dra_ex.csv')
#' }
r2dra <- function(precincts, plan, state, path) {
  if (missing(precincts)) {
    stop('precincts is a required input.')
  }
  if (missing(plan)) {
    stop('plan is a required input.')
  }
  if (missing(state)) {
    stop('state is a required input.')
  }

  if (!'sf' %in% class(precincts)) {
    stop('precincts must be an sf dataframe')
  }

  if ('character' %in% class(plan)) {
    plan <- precincts[[plan]]
  }


  shp <- tigris::blocks(state)

  if (sf::st_crs(shp) != sf::st_crs(precincts)) {
    shp <- sf::st_transform(shp, sf::st_crs(precincts))
  }

  matches <- geo_match(from = shp, to = precincts)

  dist <- plan[matches]

  out <- tibble(Id = shp$GEOID, District = dist)

  # return if no path
  if (missing(path)) {
    return(out)
  } else {
    if (stringr::str_sub(path, start = -4) == '.csv') {
      suc <- try(readr::write_csv(x = out, file = path), silent = TRUE)
    } else {
      suc <- try(readr::write_csv(x = out, file = paste0(path, '.csv')), silent = TRUE)
    }
    if (!'data.frame' %in% class(suc)) {
      warning('save to path failed. Returning dataframe to be saved manually.')
    }
  }

  out
}

globalVariables(c('District', 'District_DRA'))
