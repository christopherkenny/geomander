#' Get Harvard Election Data Archive ("HEDA") Dataset
#'
#' @param state two letter state abbreviation
#' @param path folder to put shape in. Default is \code{tempdir()}
#' @param clean_names Clean names. Default is \code{TRUE}. If \code{FALSE},
#' returns default names.
#' @param epsg `r roxy_epsg()`
#' @param ... additional arguments passed to [sf::read_sf()]
#'
#' @return sf tibble
#' @export
#'
#' @concept datasets
#' @examplesIf Sys.getenv('DATAVERSE_KEY') != ''
#' shp <- get_heda('ND')
get_heda <- function(state, path = tempdir(), clean_names = TRUE, epsg = 3857, ...) {
  abb <- tolower(censable::match_abb(state))
  
  cli::cli_inform(
    'Data sourced from the Harvard Election Data Archive {.url https://projects.iq.harvard.edu/eda/home}.',
    .frequency = 'once',
    .frequency_id = 'cite_heda'
  )
  
  if (state == 'ca') {
    # CA = block level; return tract level
    file_name <- heda_files[heda_files$state == state, ]$files[[1]]
    doi <- heda_doi()[state]
    x <- dataverse::get_dataframe_by_name(filename = file_name, dataset = doi)
    x <- x %>% 
      dplyr::mutate(GEOID = stringr::str_sub(.data$geoid10, 1, 11)) %>% 
      dplyr::group_by(.data$GEOID) %>% 
      dplyr::summarise(dplyr::across(where(is.numeric), function(x) sum(x, na.rm = TRUE)))
    
    tr <- tinytiger::tt_tracts('CA', year = 2010) %>% 
      dplyr::select(dplyr::all_of(
        c(GEOID = 'GEOID10', state = 'STATEFP10', county = 'COUNTYFP10', tract = 'TRACTCE10'))
      )
    
    out <- dplyr::left_join(x, tr, by = 'GEOID')
  } else if (state %in% c('oh', 'mn', 'il')) {
    # mn, oh, il = no zip file
    file_names <- heda_files[heda_files$state == state, ]$files[[1]] # multiple!
    doi <- heda_doi()[state]
    up_path <- Filter(function(f) stringr::str_detect(f, 'shp'), file_names)[1]
    
    lapply(file_names, function(file_name) {
      exten <- stringr::word(file_name, start = -1L, sep = fixed("."))
      tf <- tempfile(fileext = paste0('.', exten))
      dataverse::get_file_by_name(
        filename = file_name, dataset = doi,
        server = 'dataverse.harvard.edu'
      ) %>%
        writeBin(con = tf)
      file.rename(from = tf, to = paste0(tempdir(), '/', file_name))
    })
    
    out <- sf::read_sf(dsn = paste0(path, '/', up_path), ...)
    
  } else {
    file_name <- heda_files[heda_files$state == state, ]$files[[1]]
    doi <- heda_doi()[state]
    
    tf <- tempfile(fileext = '.zip')
    x <- dataverse::get_file_by_name(
      filename = file_name, dataset = doi,
      server = 'dataverse.harvard.edu'
    ) %>%
      writeBin(con = tf)
    poss <- utils::unzip(tf, list = TRUE)
    poss <- dplyr::filter(poss, !stringr::str_detect(.data$Name, 'MACOSX'),
                          stringr::str_detect(.data$Name, '.shp'))
    utils::unzip(tf, exdir = path)
    up_path <- poss$Name[1]
    
    out <- sf::read_sf(dsn = paste0(path, '/', up_path), ...)
  }
  
  if (clean_names) {
    out <- clean_heda(out)
  }
  
  make_planar_pair(out, epsg = epsg)$x
}


#' Clean HEDA Names
#'
#' @param data sf tibble from HEDA
#'
#' @return data with cleaned names
#' @export
#'
#' @concept datasets
#' @examples
#' # TODO
clean_heda <- function(data) {
  #TODO
  data
}

#' List Available States from HEDA Dataverse
#'
#' @return character abbreviations for states
#' @export
#'
#' @concept datasets
#' @examples
#' heda_states()
heda_states <- function() {
  c(
    'tx', 'ks', 'ct', 'ga', 'mn', 'md', 'wa', 'nd', 'co', 'va',
    'wy', 'nv', 'nh', 'vt', 'nc', 'nm', 'ny', 'ak', 'oh', 'hi', 'ms',
    'de', 'id', 'wi', 'in', 'ne', 'fl', 'tn', 'ma', 'sc', 'mi', 'mo',
    'al', 'az', 'nj', 'ca', 'il', 'ia', 'pa', 'la', 'sd', 'ok'
  )
}

#' Vest DOIs
#' @keywords internal
heda_doi <- function() {
  tibble::deframe(heda_files[, c('state', 'id')])
}

heda_files <- structure(
  list(
    id = c(
      '10.7910/DVN/JL8VUJ', '10.7910/DVN/WJPX3W',
      '10.7910/DVN/9ZKAIT', '10.7910/DVN/VC5TPK', '10.7910/DVN/6J2SEI',
      '10.7910/DVN/FVOXGI', '10.7910/DVN/DHWJVE', '10.7910/DVN/IZC6MA',
      '10.7910/DVN/I8KTCP', '10.7910/DVN/GAYAKU', '10.7910/DVN/C4HL1Y',
      '10.7910/DVN/BWHRVG', '10.7910/DVN/OKM0MP', '10.7910/DVN/L3QRSZ',
      '10.7910/DVN/VIAUUO', '10.7910/DVN/YF4DUC', '10.7910/DVN/AWE39N',
      '10.7910/DVN/NSAXEZ', '10.7910/DVN/G2DC8X', '10.7910/DVN/TLLQYY',
      '10.7910/DVN/AN00LH', '10.7910/DVN/VSMASQ', '10.7910/DVN/JHX654',
      '10.7910/DVN/6G4HOS', '10.7910/DVN/WYXFW3', '10.7910/DVN/IIUZHZ',
      '10.7910/DVN/BKWGVO', '10.7910/DVN/Q4GHDG', '10.7910/DVN/D3QWXB',
      '10.7910/DVN/MHEICI', '10.7910/DVN/YGOCFL', '10.7910/DVN/GFDU1N',
      '10.7910/DVN/UUCWPP', '10.7910/DVN/HTUGFD', '10.7910/DVN/KX0YGR',
      '10.7910/DVN/Y74SD3', '10.7910/DVN/USCMPG', '10.7910/DVN/VX9KUW',
      '10.7910/DVN/FJHHDS', '10.7910/DVN/LSMM0T', '10.7910/DVN/LFNKS4',
      '10.7910/DVN/LXZRMG'
    ),
    files = list(
      'Texas_Shapefile.zip', 'ks_shapefile.zip',
      'CT_Shapefile.zip', 'Georgia_Shapefiles.zip', c(
        'MN_final.dbf',
        'MN_final.sbn', 'MN_final.sbx', 'MN_final.shp', 'MN_final.shx'
      ), 'MD Data.zip', 'WA_Shapefile.zip', 'ND_Shapefile.zip',
      'CO_Shapefile.zip', 'VA Data.zip', 'WY_Shapefile.zip', 'nv_shapefile.zip',
      'NH_Shapefile.zip', 'vt_shapefile.zip', 'NC_Shapefiles.zip',
      'NM_Shapefile.zip', 'ny_shapefile.zip', 'AK_Shapefile.zip',
      c(
        'OH_final.dbf', 'OH_final.sbn', 'OH_final.sbx', 'OH_final.shp',
        'OH_final.shx'
      ), 'HI_Shapefile.zip', 'MS_Shapefile.zip',
      'de_shapefile.zip', 'ID_Shapefile.zip', 'Wisconsin_Shapefiles.zip',
      'IN_Shapefile.zip', 'NE_Shapefile.zip', 'FL_Shapefile.zip',
      'TN_Shapefile.zip', 'MA_Shapefile.zip', 'South_Carolina_Shapefiles.zip',
      'MI_Shapefiles.zip', 'MO_Shapefile.zip', 'AL_Shapefile.zip',
      'AZ_Shapefile.zip', 'NJ_Shapefile.zip', 'CA_2008_2010.tab',
      c(
        'IL_final.dbf', 'IL_final.sbn', 'IL_final.sbx', 'IL_final.shp',
        'IL_final.shx'
      ), 'ia_shapefile.zip', 'PA_Shapefile.zip',
      'la_shapefile.zip', 'SD_Shapefile.zip', 'OK_Shapefile.zip'
    ),
    state = c(
      'tx', 'ks', 'ct', 'ga', 'mn', 'md', 'wa', 'nd',
      'co', 'va', 'wy', 'nv', 'nh', 'vt', 'nc', 'nm', 'ny', 'ak',
      'oh', 'hi', 'ms', 'de', 'id', 'wi', 'in', 'ne', 'fl', 'tn',
      'ma', 'sc', 'mi', 'mo', 'al', 'az', 'nj', 'ca', 'il', 'ia',
      'pa', 'la', 'sd', 'ok'
    )
  ),
  row.names = c(NA, -42L),
  class = c(
    'tbl_df',
    'tbl', 'data.frame'
  )
)

#' HEDA Parties
#' @keywords internal
heda_party <- function(str) {
  p <- stringr::str_sub(str, 7, 7)
  if (p == 'r') {
    p <- 'rep'
  } else if (p == 'd') {
    p <- 'dem'
  } else if (p == 'l') {
    p <- 'lib'
  } else if (p == 'i') {
    p <- 'ind'
  } else if (p == 'dr') {
    p <- 'der'
  } else if (p == 'p') {
    p <- 'pro'
  } else {
    p <- 'unk'
  }

  p
}

heda_abb <- structure(
  list(
    a = c(
      'USP', 'USS', 'USH', 'GOV', 'LTG', 'ATG',
      'SOS', 'TRE', 'STS', 'STH', 'ADJ', 'AGR', 'AUD', 'COM', 'INS',
      'LND', 'RGNT', 'SPI', 'SC#', 'SCC', 'CCA', 'CCA#', 'RR#', 'SBOE',
      'SPI', 'CFO', 'COC', 'CCJ#', 'PSC#', 'CVA', 'FRE#', 'LBR', 'MAY',
      'DEL', 'SHADS', 'SHADR', 'STH2', 'STHa'
    ),
    b = c(
      'U.S. President',
      'U.S. Senate', 'U.S. House', 'Governor', 'Lieutenant Governor',
      'Attorney General', 'Secretary of State', 'State Treasurer',
      'State Senate (upper house)', 'State House / Assembly (lower house)',
      'Adjutant General(SC)', 'Agriculture Commissioner / Secretary',
      'Auditor', 'Comptroller / Controller', 'Insurance Commissioner',
      'Land Commissioner / Commissioner of Public Lands', 'Regent',
      'Superintendent of Education', 'State Supreme Court, # is seat number, Ex ”SC1” (TX)',
      'State Supreme Court Chief Justice (TX)', 'State Circuit Courts of Appeals (LA)',
      'State Criminal Court of Appeals, # is seat number, “PJ” is “Presiding Judge”. Ex. “CCA1”; “CCAPJ” (TX)',
      'State Railroad Commission, # is seat number, Ex. “RR1” (TX)',
      'State Board of Education (TX)', 'Superintendent of Public Instruction',
      'Chief Financial Officer (FL)', 'Chairman of the Council (DC)',
      'Circuit Court Judge, # is seat number', 'Public Service Commission, # is seat number',
      'Court of Civil Appeals(AL)', 'County Freeholder (NJ), # is seat number',
      'Labor Commissioner (OK)', 'Mayor (DC)', 'Delegate to USH (DC)',
      'Shadow Senator (DC)', 'Shadow Representative (DC)', 'Second State House Contest (for NJ)',
      'Average State House Results (for NJ)'
    )
  ),
  class = c('tbl_df', 'tbl', 'data.frame'),
  row.names = c(NA, -38L)
)
