library(tidyverse)
library(dataverse)

contents <- dataverse::dataverse_contents('eda')
ids <- sapply(contents, \(y) y$identifier)
ids <- Filter(\(x) !x %in% c('DVN/EQCMZM', 'DVN/DDWGUI'), ids) # MN and UT are deacc.
ids <- paste0('10.7910/', ids)
# dataverse::dataset_metadata(paste0('10.7910/', ids[1]))

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
  unnest(state) %>%
  mutate(state = tolower(state))

eda_save <- eda %>%
  filter(!is.na(state)) %>%
  mutate(files = map(files, \(x) Filter(\(y) stringr::str_detect(y, '.zip'), x)))

eda_aug <- tibble::tribble(
  ~state, ~files,
  'ca', 'CA_2008_2010.tab',
  'mn', c('MN_final.dbf', 'MN_final.sbn', 'MN_final.sbx', 'MN_final.shp', 'MN_final.shx'),
  'oh', c('OH_final.dbf', 'OH_final.sbn', 'OH_final.sbx', 'OH_final.shp', 'OH_final.shx'),
  'il', c('IL_final.dbf', 'IL_final.sbn', 'IL_final.sbx', 'IL_final.shp', 'IL_final.shx')
)

eda_save <- rows_update(eda_save, eda_aug, by = 'state')
# dput(eda_save)

ND_files <- eda %>%
  filter(id == '10.7910/DVN/YN4TLR') %>%
  pull(files) %>%
  .[[1]] %>%
  Filter(\(x) substr(x, 1, 2) == 'ND', .)

# dataverse::dataset_files(ids[10])

tf <- tempfile(fileext = '.zip')
x <- dataverse::get_file_by_name('ND_Shapefile.zip', ids[10]) %>%
  writeBin(con = tf)
utils::unzip(tf, exdir = tempdir())
shp <- sf::st_read(paste0(tempdir(), '/ND_final.shp'))

tabs <- lapply(
  ND_files,
  function(f) dataverse::get_dataframe_by_name(f, '10.7910/DVN/YN4TLR')
)
