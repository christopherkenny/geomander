library(tidyverse)
library(dataverse)

contents <- dataverse::dataverse_contents('eda')
ids <- sapply(contents, \(y) y$identifier)
ids <- Filter(\(x) !x %in% c('DVN/EQCMZM', 'DVN/DDWGUI'), ids) # MN and UT are deacc.
ids <- paste0('10.7910/', ids)
#dataverse::dataset_metadata(paste0('10.7910/', ids[1]))

out <- lapply(
  ids, 
  \(id) dataverse::dataset_metadata(id)
)

ns <- sapply(out, \(x) x$fields$value[[1]])

all_files <- lapply(ids, function(id) {
                      id %>% 
    dataverse::dataset_files() %>% 
    purrr::map_chr(\(z) z$label)
})

eda <- tibble::tibble(
  id = ids, name = ns, files = all_files
) %>% 
  mutate(
    state = ifelse(
      str_detect(name, ' Data Files'), 
      sapply(name, \(n) censable::match_abb(str_remove(n, ' Data Files'))), 
      NA_character_
      )
  ) %>% 
  unnest(state)

eda_states <- function() {
  c("tx", "ks", "ct", "ga", "mn", "md", "wa", "nd", "co", "va", 
    "wy", "nv", "nh", "vt", "nc", "nm", "ny", "ak", "oh", "hi", "ms", 
    "de", "id", "wi", "in", "ne", "fl", "tn", "ma", "sc", "mi", "mo", 
    "al", "az", "nj", "ca", "il", "ia", "pa", "la", "sd", "ok")
}

eda_abb <- function() {
  tibble::tribble(
    ~a, ~b,
    'USP', 'U.S. President',
    'USS', 'U.S. Senate',
    'USH', 'U.S. House',
    'GOV', 'Governor',
    'LTG', 'Lieutenant Governor',
    'ATG', 'Attorney General',
    'SOS', 'Secretary of State',
    'TRE', 'State Treasurer',
    'STS', 'State Senate (upper house)',
    'STH', 'State House / Assembly (lower house)',
    'ADJ', 'Adjutant General(SC)',
    'AGR', 'Agriculture Commissioner / Secretary',
    'AUD', 'Auditor',
    'COM', 'Comptroller / Controller',
    'INS', 'Insurance Commissioner',
    'LND', 'Land Commissioner / Commissioner of Public Lands',
    'RGNT', 'Regent',
    'SPI', 'Superintendent of Education',
    'SC#', 'State Supreme Court, # is seat number, Ex ”SC1” (TX)',
    'SCC',  'State Supreme Court Chief Justice (TX)',
    'CCA', 'State Circuit Courts of Appeals (LA)',
    'CCA#', 'State Criminal Court of Appeals, # is seat number, “PJ” is “Presiding Judge”. Ex. “CCA1”; “CCAPJ” (TX)',
    'RR#', 'State Railroad Commission, # is seat number, Ex. “RR1” (TX)',
    'SBOE', 'State Board of Education (TX)',
    'SPI', 'Superintendent of Public Instruction',
    'CFO', 'Chief Financial Officer (FL)',
    'COC', 'Chairman of the Council (DC)',
    'CCJ#', 'Circuit Court Judge, # is seat number',
    'PSC#', 'Public Service Commission, # is seat number',
    'CVA', 'Court of Civil Appeals(AL)',
    'FRE#', 'County Freeholder (NJ), # is seat number',
    'LBR', 'Labor Commissioner (OK)',
    'MAY', 'Mayor (DC)',
    'DEL', 'Delegate to USH (DC)',
    'SHADS', 'Shadow Senator (DC)',
    'SHADR', 'Shadow Representative (DC)',
    'STH2', 'Second State House Contest (for NJ)',
    'STHa', 'Average State House Results (for NJ)'
  )
}

AK_files <- eda %>% 
  filter(id == '10.7910/DVN/YN4TLR') %>% 
  pull(files) %>% 
  .[[1]] %>% 
  Filter(\(x) substr(x, 1, 2) == 'AK', .)

#dataverse::dataset_files(ids[10])

tf <- tempfile(fileext = '.zip')
x <- dataverse::get_file_by_name('ND_Shapefile.zip', ids[10]) %>%
  writeBin(con = tf)
utils::unzip(tf, exdir = tempdir())
shp <- sf::st_read(paste0(tempdir(), '/ND_final.shp'))

