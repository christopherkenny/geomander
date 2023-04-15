# Notes on HEDA files
#
#
# - CT: Registration counts for Connecticut, as of 2008


#' Get Harvard Election Data Archive ("HEDA") Dataset
#'
#' @param state two letter state abbreviation
#' @param path folder to put shape in. Default is \code{tempdir()}
#' @param epsg `r roxy_epsg()`
#' @param ... additional arguments passed to [sf::read_sf()]
#'
#' @return sf tibble
#' @export
#'
#' @concept datasets
#' @examplesIf Sys.getenv('DATAVERSE_KEY') != ''
#' shp <- get_heda('ND')
get_heda <- function(state, path = tempdir(), epsg = 3857, ...) {
  clean_names <- FALSE
  abb <- tolower(censable::match_abb(state))

  match.arg(abb, choices = heda_states())

  cli::cli_inform(
    'Data sourced from the Harvard Election Data Archive {.url https://projects.iq.harvard.edu/eda/home}.',
    .frequency = 'once',
    .frequency_id = 'cite_heda'
  )

  if (abb == 'ca') {
    # CA = block level; return tract level
    file_name <- heda_files[heda_files$state == abb, ]$files[[1]]
    doi <- heda_doi()[abb]
    x <- dataverse::get_dataframe_by_name(filename = file_name, dataset = doi)
    x <- x %>%
      dplyr::mutate(GEOID = stringr::str_sub(.data$geoid10, 1, 11)) %>%
      dplyr::group_by(.data$GEOID) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), function(x) sum(x, na.rm = TRUE)))

    tr <- tinytiger::tt_tracts('CA', year = 2010) %>%
      dplyr::select(dplyr::all_of(
        c(GEOID = 'GEOID10', state = 'STATEFP10', county = 'COUNTYFP10', tract = 'TRACTCE10')
      ))

    out <- dplyr::left_join(x, tr, by = 'GEOID') %>%
      sf::st_as_sf()
  } else if (abb %in% c('oh', 'mn', 'il')) {
    # mn, oh, il = no zip file
    file_names <- heda_files[heda_files$state == abb, ]$files[[1]] # multiple!
    doi <- heda_doi()[abb]
    up_path <- Filter(function(f) stringr::str_detect(f, 'shp'), file_names)[1]

    lapply(file_names, function(file_name) {
      exten <- stringr::word(file_name, start = -1L, sep = stringr::fixed('.'))
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
    if (abb %in% c('ga', 'va')) {
      restore_shx <- Sys.getenv('SHAPE_RESTORE_SHX')
      Sys.setenv(SHAPE_RESTORE_SHX = 'YES')
      on.exit({
        Sys.setenv(SHAPE_RESTORE_SHX = restore_shx)
      })
    }
    file_name <- heda_files[heda_files$state == abb, ]$files[[1]]
    doi <- heda_doi()[abb]

    tf <- tempfile(fileext = '.zip')
    x <- dataverse::get_file_by_name(
      filename = file_name, dataset = doi,
      server = 'dataverse.harvard.edu'
    ) %>%
      writeBin(con = tf)
    poss <- utils::unzip(tf, list = TRUE)
    poss <- dplyr::filter(
      poss, !stringr::str_detect(.data$Name, 'MACOSX'),
      stringr::str_detect(.data$Name, '.shp')
    )
    utils::unzip(tf, exdir = path)
    up_path <- poss$Name[1]
    if (abb == 'va') {
      file.rename(paste0(path, '/', up_path), to = paste0(path, '/', stringr::str_sub(up_path, end = -12)))
      up_path <- stringr::str_sub(up_path, end = -12)
    }

    out <- sf::read_sf(dsn = paste0(path, '/', up_path), ...)
  }

  # unless stated, they seem to use 4140
  if (is.na(sf::st_crs(out))) {
    sf::st_crs(out) <- 4140
  }

  if (clean_names) {
    if (state %in% c('ny')) {
      out <- clean_heda(out, state)
    } else {
      out <- clean_heda(out)
    }
  }

  make_planar_pair(out, epsg = epsg)$x
}


#' Clean HEDA Names
#'
#' @param data sf tibble from HEDA
#' @param state bypass regular path with state specific changes
#'
#' @return data with cleaned names
#' @noRd
clean_heda <- function(data, state) {
  # custom: az ny co fl
  # auto: ak al ca de ct
  # todo:   ga hi ia id il in ks la ma md mi mn mo ms nc nd ne nh nj nm nv oh ok pa sc sd tn tx vt wa wi wy
  if (missing(state)) {
    # normal track
    data <- data %>%
      dplyr::select(
        -dplyr::ends_with('_1'),
        -dplyr::any_of(c(
          'VTDI10', 'NAME10', 'NAMELSAD10', 'LSAD10',
          'MTFCC10', 'FUNCSTAT10', 'ALAND10',
          'AWATER10', 'INTPTLAT10', 'INTPTLON10'
        ))
      )

    data <- data %>%
      dplyr::rename(
        dplyr::any_of(
          c(
            GEOID = 'GEOID10', state = 'STATEFP10', county = 'COUNTYFP10',
            tract = 'TRACTCE10', vtd = 'VTDST10', precinct = 'PRECINCT',
            county = 'CNTYKEY', vtd = 'VTDKEY'
          )
        )
      ) %>%
      dplyr::select(
        dplyr::any_of(c('GEOID', 'state', 'county', 'tract', 'vtd', 'precinct')),
        dplyr::ends_with(c(
          '_00', '_01', '_02', '_03', '_04', '_05', '_06', '_07', '_08',
          '_09', '_10', '_11', '_12', '_13', '_14', '_votes'
        )),
        dplyr::any_of(c('NDV', 'NRV', ndv = 'NV_D', nrv = 'NV_R'))
      ) %>%
      dplyr::select(-dplyr::matches('^[a-zA-Z]{3}_\\d{2}$')) %>%
      dplyr::select(-dplyr::matches('^[a-zA-Z]{1}_\\d{2}$')) %>%
      dplyr::rename_with(.fn = stringr::str_to_lower, .cols = -dplyr::any_of('GEOID')) %>%
      dplyr::rename_with(.fn = function(x) stringr::str_replace(string = x, pattern = '_tot_', '_')) %>%
      dplyr::select(-dplyr::contains('_reg_'), -dplyr::ends_with('_pct')) %>%
      dplyr::rename_with(.fn = \(x) stringr::str_remove(x, pattern = '_votes'), dplyr::ends_with('_votes')) %>%
      dplyr::rename_with(.fn = \(x) stringr::str_replace(x, '_20', '_'), dplyr::matches('\\d{4}'))

    noms <- names(data)
    elec <- which(stringr::str_count(string = noms, '_') == 2)
    if (length(elec) > 0 && stringr::str_sub(noms[elec[1]], 1, 3) %in% c('dem', 'rep')) {
      for (i in seq_along(elec)) {
        party <- stringr::str_sub(noms[elec[i]], 1, 3)
        yr <- stringr::str_extract(noms[elec[i]], '\\d+')
        off <- stringr::str_sub(noms[elec[i]], 5, 7)
        off <- ifelse(
          is.na(heda_abb_from_alarm[off]),
          off,
          heda_abb_from_alarm[off]
        )
        noms[elec[i]] <- stringr::str_glue('{off}_{yr}_{party}')
      }
    } else {
      for (i in seq_along(elec)) {
        off <- stringr::str_sub(stringr::str_extract(noms[elec[i]], '^(.+?)_'), end = -2L)
        off <- ifelse(
          is.na(heda_abb_from_alarm[off]),
          off,
          heda_abb_from_alarm[off]
        )

        yr <- stringr::str_extract(noms[elec[i]], '\\d+')
        party <- heda_party(noms[elec[i]])
        noms[elec[i]] <- stringr::str_glue('{off}_{yr}_{party}')
      }
    }

    names(data) <- noms
  } else {
    if (state == 'ny') {
      data <- data %>%
        dplyr::select(
          c(
            GEOID = 'GEOID10', state = 'STATEFP10', county = 'COUNTYFP10', vtd = 'VTDST10',
            elect_id = 'ELECT_ID', gov_10_dem = 'GOV_DVOTE_', gov_10_rep = 'GOV_RVOTE_',
            com_10_dem = 'COMP_DVOTE', com_10_rep = 'COMP_RVOTE',
            atg_10_dem = 'AG_DVOTE_1', atg_10_rep = 'AG_RVOTE_1',
            uss_10_dem_gil = 'USS_2_DVOT', uss_10_rep_dio = 'USS_2_RVOT',
            uss_10_dem_sch = 'USS_6_DVOT', uss_10_rep_tow = 'USS_6_RVOT',
            ndv = 'NDV', nrv = 'NRV', 'geometry'
          )
        )
    } else if (state == 'az') {
      data <- data %>%
        dplyr::select(
          c(
            GEOID = 'GEOID10', state = 'STATEFP10', county = 'COUNTYFP10', vtd = 'VTDST10',
            pre_08_rep = 'PRS08_REP', pre_08_dem = 'PRS08_DEM', pre_08_oth = 'PRS08_OTH',
            gov_10_dem = 'GOV10_DEM', gov_10_rep = 'GOV10_REP', gov_10_oth = 'GOV10_OTH',
            sos_10_dem = 'SOS_10DEM', sos_10_rep = 'SOS_10REP', sos_10_oth = 'SOS_10OTH',
            atg_10_dem = 'AG10_DEM', atg_10_rep = 'AG10_REP', atg_10_oth = 'AG1_0OTH',
            tre_10_dem = 'ST10_DEM', tre_10_rep = 'ST10_REP', tre_10_oth = 'ST10_OTH',
            spi_10_dem = 'SPI10_DEM', spi_10_rep = 'SPI10_REP', spi_10_oth = 'SPI10_OTH',
            uss_10_dem = 'USSEN10_DE', uss_10_rep = 'USSEN10_RE', uss_10_oth = 'USSEN10_OT',
            ndv = 'NDV', nrv = 'NRV', 'geometry'
          )
        )
    } else if (state == 'co') {
      data <- data %>%
        dplyr::select(
          c(
            c(
              GEOID = 'GEOID10', state = 'STATEFP10', county = 'COUNTYFP10', vtd = 'VTDST10',
              pre_08_dem = 'PRES08__D', pre_08_rep = 'PRES08__R', pre_08_oth = 'PRES08_MP',
              uss_08_dem = 'USSEN08_D', uss_08_rep = 'USSEN08_R', uss_08_oth = 'USSEN08_MP',
              ush_08_rep = 'USHSE08_R', ush_08_dem = 'USHSE08_D',
              rgn_08_dem = 'RGNT08_D', rgn_08_rep = 'RGNT08_R',
              sbe_08_dem = 'SBE08_D', sbe_08_rep = 'SBE08_R',
              ssd_08_rep = 'SD08_R', ssd_08_dem = 'SD08_D',
              shd_08_dem = 'HD08_D', shd_08_rep = 'HD08_R',
              uss_10_rep = 'USSEN10R', uss_10_dem = 'USSEN10D', uss_10_oth = 'USSEN10MP',
              gov_10_rep = 'GOV10R', gov_10_dem = 'GOV10D', gov_10_oth = 'GOVMP',
              sos_10_rep = 'SOS10R', sos_10_dem = 'SOS10D', sos_10_oth = 'SOS10MP',
              atg_10_rep = 'AG10R', atg_10_dem = 'AG10D',
              tre_10_rep = 'TRE10R', tre_10_dem = 'TRE10D',
              rgl_10_rep = 'RGNT10LRGR', rgl_10_dem = 'RGNT10LRGD', rgl_10_oth = 'RGNT10LRGM',
              ush_10_rep = 'USHSE10R', ush_10_dem = 'USHSE10D', ush_10_oth = 'USHSE10MP',
              rgn_10_rep = 'RGNT10R', rgn_10_dem = 'RGNT10_D',
              boe_10_rep = 'SBE10R', boe_10_dem = 'SBE10D',
              ssd_10_rep = 'SD10_R', ssd_10_dem = 'SD10_D',
              shd_10_dem = 'HD10_D', shd_10_rep = 'HD10_R',
              ndv = 'NDV', nrv = 'NRV', 'geometry'
            )
          )
        )
    } else if (state == 'fl') {
      data <- data %>%
        dplyr::select(
          c(
            county = 'COUNTY', vtd = 'precinct',
            gov_10_dem_sin = 'GOV_D_SINK', gov_10_rep_sco = 'GOV_R_SCOT', gov_10_oth_all = 'GOV_NPA_AL',
            gov_10_oth_art = 'GOV_NPA_AR', gov_10_oth_imp = 'GOV_NPA_IM', gov_10_oth_kha = 'GOV_NPA_KH',
            gov_10_oth_ree = 'GOV_NPA_RE',
            cfo_10_dem_aus = 'CFO_D_AUSL', cfo_10_rep_atw = 'CFO_R_ATWA', cfo_10_oth_maz = 'CFO_NPA_MA',
            cfo_10_oth_ste = 'CFO_NPA_ST',
            atg_10_dem_gel = 'AG_D_GELBE', atg_10_rep_bon = 'AG_R_BONDI',
            atg_10_oth_lew = 'AG_NPA_LEW',
            agc_10_dem_mad = 'AGC_D_MADD', agc_10_rep_put = 'AGC_R_PUTN',
            agc_10_tea_che = 'AGC_TEA_CH', agc_10_oth_ham = 'AGC_NPA_HA',
            uss_10_dem_mee = 'SEN_D_MEEK', uss_10_rep_rub = 'SEN_R_RUBI', uss_10_oth_arm = 'SEN_NPA_AR',
            uss_10_oth_ask = 'SEN_NPA_AS', uss_10_oth_dec = 'SEN_NPA_DE', uss_10_oth_cri = 'SEN_NPA_CR',
            uss_10_oth_rig = 'SEN_NPA_RI', uss_10_lib_sni = 'SEN_LBT_SN',
            ndv = 'NDV', nrv = 'NRV', 'geometry'
          )
        )
    } else if (state == 'ga') {
      data <- data %>%
        dplyr::select(
          c(
            'ID', 'AREA', 'DATA', 'CTYSOSID', 'PRECINCT_C', 'PRECINCT_N',
            'COUNTY_NAM', 'FIPS', 'FIPS2', 'COUNTY_NUM',
            'MCCAIN08', 'OBAMA08', 'BARR08',
            'CHAMBLISS0', 'MARTIN08', 'BUCKLEY08', 'MCDONALD08', 'POWELL08',
            'GIVENS08', 'EVERETT08', 'MONDS08', 'CREP08', 'CDEM08', 'STSREP08',
            'STSDEM08', 'STHREP08', 'STHDEM08',
            'GOV_RVOTE_', 'GOV_DVOTE_', 'GOV_LIBVOT',
            'LTG_RVOTE_', 'LTG_DVOTE_', 'BUCKLEY06', 'SOS_RVOTE_', 'SOS_DVOTE_',
            'SOS_LIBVOT', 'AG_RVOTE_0', 'AG_DVOTE_0', 'BLACK06', 'IRVIN06',
            'CASHIN06', 'OXENDINE06', 'DREXINGER0', 'SUPT_RVOTE', 'SUPT_DVOTE',
            'SUPT_LIBVO', 'AGR_RVOTE_', 'AGR_DVOTE_', 'PSC3_DVOTE', 'PSC3_RVOTE',
            'PSC3_LIBVO', 'PSC5_RVOTE', 'PSC5_DVOTE', 'PSC5_LIBVO', 'USH_RVOTE_',
            'USH_DVOTE_', 'STS_RVOTE_', 'STS_DVOTE_', 'STH_RVOTE_', 'STH_DVOTE_',
            'CNTYSOSID_', 'PRECINCT_0',
            'PRES_RVOTE', 'PRES_DVOTE', 'USS_RVOTE_', 'USS_DVOTE_', 'USS_LIBVOT',
            'PSC4_RVOTE', 'PSC4_DVOTE', 'PSC4_LIBVO', 'PSC1_RVOTE', 'PSC1_LIBVO',
            'USH_RVOTE0', 'USH_DVOTE0', 'STH_RVOTE0', 'STH_DVOTE0',
            'geometry'
          )
        )
    }
  }
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
    'tx', 'ks', 'ct', 'ga', 'mn', 'md', 'wa', 'nd', 'co',
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
      '10.7910/DVN/I8KTCP', '10.7910/DVN/C4HL1Y',
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
      'CO_Shapefile.zip', 'WY_Shapefile.zip', 'nv_shapefile.zip',
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
      'co', 'wy', 'nv', 'nh', 'vt', 'nc', 'nm', 'ny', 'ak',
      'oh', 'hi', 'ms', 'de', 'id', 'wi', 'in', 'ne', 'fl', 'tn',
      'ma', 'sc', 'mi', 'mo', 'al', 'az', 'nj', 'ca', 'il', 'ia',
      'pa', 'la', 'sd', 'ok'
    )
  ),
  row.names = c(NA, -41L),
  class = c(
    'tbl_df',
    'tbl', 'data.frame'
  )
)

#' HEDA Parties
#' @keywords internal
heda_party <- function(str) {
  p <- stringr::str_extract(str, '_._')
  if (p == '_r_') {
    p <- 'rep'
  } else if (p == '_d_') {
    p <- 'dem'
  } else if (p == '_l_') {
    p <- 'lib'
  } else if (p == '_i_') {
    p <- 'ind'
  } else if (p == '_dr_') {
    p <- 'dmr' # democratic republicans
  } else if (p == '_p_') {
    p <- 'pro' # progressives
  } else if (p == '_tot_') {
    p <- 'tot'
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
      'Superintendent of Education', 'State Supreme Court, # is seat number, Ex SC1 (TX)',
      'State Supreme Court Chief Justice (TX)', 'State Circuit Courts of Appeals (LA)',
      'State Criminal Court of Appeals, # is seat number, PJ is Presiding Judge. Ex. CCA1; CCAPJ (TX)',
      'State Railroad Commission, # is seat number, Ex. RR1 (TX)',
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

heda_abb_from_alarm <- tibble::tribble(
  ~heda, ~alarm,
  'USP', 'pre',
  'prs', 'pre',
  'pre', 'pre',
  'pres', 'pre',
  'USS', 'uss',
  'USH', 'ush',
  'GOV', 'gov',
  'LTG', 'ltg',
  'lt', 'ltg',
  'ltgov', 'ltg',
  'ATG', 'atg',
  'ag', 'atg',
  'SOS', 'sos',
  'TRE', 'tre',
  'STS', 'ssd',
  'STH', 'shd',
  'ADJ', 'adj',
  'AGR', 'agc',
  'AUD', 'aud',
  'audit', 'aud',
  'COM', 'com',
  'INS', 'ins',
  'LND', 'lnd',
  'RGNT', 'rgn',
  'SPI', 'soe',
  'SC#', 'spc',
  'SCC', 'spc',
  'CCA', 'jud',
  'CCA#', 'jud',
  'RR#', 'rrd', # railroad
  'SBOE', 'boe',
  'SPI', 'spi',
  'CFO', 'cfo',
  'COC', 'coc',
  'CCJ#', 'ccj',
  'PSC#', 'psc',
  'CVA', 'cva',
  'FRE#', 'fre',
  'LBR', 'lbr',
  'MAY', 'may',
  'DEL', 'ush', # DC
  'SHADS', 'ssd', # DC
  'SHADR', 'ush', # DC
  'STH2', 'shd',
  'STHa', 'shd',
  'sen', 'ssd',
  'cng', 'ush',
  'trs', 'tre',
  'treas', 'tre',
  'con', 'con' # CA controller ...
) %>%
  dplyr::mutate(heda = tolower(.data$heda)) %>%
  tibble::deframe()
