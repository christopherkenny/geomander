
#' Get VEST Dataset
#'
#' @param state two letter state abbreviation
#' @param year year in 2016, 2018, or 2020
#' @param path folder to put shape in. Default is \code{tempdir()}
#' @param clean_names Clean names. Default is \code{TRUE}. If \code{FALSE}, 
#' returns default names.
#'
#' @return sf tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires Dataverse API
#' shp <- get_vest('CO', 2020)
#' }
get_vest <- function(state, year, path = tempdir(), clean_names = TRUE) {
  abb <- tolower(censable::match_abb(state))
  
  file_name <- stringr::str_glue('{abb}_{year}.zip')
  
  doi <- vest_doi()[as.character(year)]
  
  
  tf <- tempfile(fileext = '.zip')
  x <- dataverse::get_file_by_name(filename = file_name, dataset = doi, 
                              server = 'dataverse.harvard.edu') %>% 
    writeBin(con = tf)
  zip::unzip(tf, exdir = path)
  
  poss <-  sf::st_layers(dsn = path)[[1]]
  up_path <- poss[stringr::str_starts(string = poss, stringr::str_glue('{abb}_{year}'))]
  
  out <- sf::st_read(dsn = paste0(path, '/', up_path,'.shp'))
  
  if (clean_names) {
    out <- out %>% clean_vest()
  }
  
  out
}


#' Clean Vest Names
#' 
#' @param data sf tibble from VEST
#' 
#' @return data with cleaned names
#' @export
#' @examples 
#' data(va18sub)
#' va <- clean_vest(va18sub)
clean_vest <- function(data) {
  
  noms <- names(data)
  
  gen <- grep('G[0-9]{2}', noms, value = FALSE) # General
  run <- grep('R[0-9]{2}', noms, value = FALSE) # Runoff; necessary for LA/GA/etc
  
  for (i in seq_along(gen)) {
    off <- tolower(stringr::str_sub(noms[gen[i]], 4, 6))
    yr <- stringr::str_sub(noms[gen[i]], 2, 3)
    party <- vest_party(noms[gen[i]])
    noms[gen[i]] <- stringr::str_glue('{off}_{yr}_{party}')
  }
  
  for (i in seq_along(run)) {
    off <- tolower(stringr::str_sub(noms[gen[i]], 4, 6))
    yr <- paste0('r', stringr::str_sub(noms[gen[i]], 2, 3))
    party <- vest_party(noms[gen[i]])
    noms[run[i]] <- stringr::str_glue('{off}_{yr}_{party}')
  }
  
  names(data) <- noms
  
  data
} 

#' List Available States from VEST Dataverse
#'
#' @param year year in 2016, 2018, or 2020
#'
#' @return character abbreviations for states
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires Dataverse API
#' vest_states(2020)
#' }
vest_states <- function(year) {
  doi <- vest_doi()[as.character(year)]
  files <- dataverse::dataset_files(doi, server = 'dataverse.harvard.edu')
  out <- stringr::str_sub(unlist(lapply(files, function(x) { x[['label']] } )), 1, 2)
  base::setdiff(out, 'do')
}

#' Vest DOIs
#' @keywords internal
vest_doi <- function(){
  c(`2020` = 'doi:10.7910/DVN/K7760H', 
    `2018` = 'doi:10.7910/DVN/UBKYRU', 
    `2016` = 'doi:10.7910/DVN/NH5S2I')
}

#' Vest Parties
#' @keywords internal
vest_party <- function(str) {
  p <- stringr::str_sub(str, 7, 7)
  if (p == 'R') {
    p <- 'rep'
  } else if (p == 'D') {
    p <- 'dem'
  } else if (p == 'L') {
    p <- 'lib'
  } else if (p == 'G') {
    p <- 'gre'
  } else if (p == 'O') {
    p <- 'oth'
  } else if (p == 'I') {
    p <- 'ind'
  } else if (p == 'I') {
    p <- 'ind'
  } else if (p == 'C') {
    p <- 'con'
  } else {
    p <- 'unk'
  }
  
  p
}


#' Vest Abbreviations
#' @keywords internal
vest_abb <- function(x) {
  structure(list(a = c("A##", "AGR", "ATG", "AUD", "CFO", 
                       "CHA", "COC", "COM", "CON", "COU", "CSC", "DEL", "GOV", 
                       "H##", "HOD", "HOR", "INS", "LAB", "LND", "LTG", "MAY", 
                       "MNI", "PSC", "PUC", "RGT", "SAC", "SBE", "SCC", "SOC", 
                       "SOS", "SPI", "SPL", "SSC", "TAX", "TRE", "UBR", "USS"
  ), 
  b = c("Ballot amendment, where ## is an identifier", "Commissioner of Agriculture", 
        "Attorney General", "Auditor", "Chief Financial Officer", 
        "Council Chairman", "Corporation Commissioner", "Comptroller", 
        "State Controller", "City Council Member", "Clerk of the Supreme Court", 
        "Delegate to the U.S. House", "Governor", "U.S. House, where ## is the district number. AL: at large.", 
        "House of Delegates, accompanied by a HOD_DIST column indicating district number", 
        "U.S. House, accompanied by a HOR_DIST column indicating district number", 
        "Insurance Commissioner", "Labor Commissioner", "Commissioner of Public/State Lands", 
        "Lieutenant Governor", "Mayor", "State Mine Inspector", "Public Service Commissioner", 
        "Public Utilities Commissioner", "State University Regent", 
        "State Appeals Court (in AL: Civil Appeals)", "State Board of Education", 
        "State Court of Criminal Appeals", "Secretary of Commonwealth", 
        "Secretary of State", "Superintendent of Public Instruction", 
        "Commissioner of School and Public Lands", "State Supreme Court", 
        "Tax Commissioner", "Treasurer", "University Board of Regents/Trustees/Governors", 
        "U.S. Senate")), 
  row.names = c(NA, -37L), 
  class = c("tbl_df", 
            "tbl", "data.frame"))
}