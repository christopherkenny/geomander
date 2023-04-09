test_that('split works', {
  low <- checkerboard %>% dplyr::slice(1:3, 9:11)
  prec <- checkerboard %>%
    dplyr::slice(1:3) %>%
    dplyr::summarize(geometry = sf::st_union(geometry))
  dists <- checkerboard %>%
    dplyr::slice(1:3, 9:11) %>%
    dplyr::mutate(dist = c(1, 2, 2, 1, 3, 3)) %>%
    dplyr::group_by(dist) %>%
    dplyr::summarize(geometry = sf::st_union(geometry))

  a <- split_precinct(low, prec, dists, split_by_id = 'dist', epsg = FALSE)

  e <- structure(
    list(
      new = 1:2,
      geometry = structure(
        list(
          structure(
            list(
              structure(c(0, 1, 1, 0, 0, 0, 0, 1, 1, 0),
                .Dim = c(5L, 2L)
              )
            ),
            class = c('XY', 'POLYGON', 'sfg')
          ),
          structure(list(structure(c(1, 1, 0, 0, 0, 1, 1, 2, 1, 1, 2, 3, 3, 2), .Dim = c(7L, 2L))),
            class = c('XY', 'POLYGON', 'sfg')
          )
        ),
        class = c('sfc_POLYGON', 'sfc'),
        precision = 0, bbox = structure(c(xmin = 0, ymin = 0, xmax = 1, ymax = 3), class = 'bbox'),
        crs = structure(list(input = NA_character_, wkt = NA_character_), class = 'crs'),
        classes = c('GEOMETRYCOLLECTION', 'GEOMETRYCOLLECTION'), n_empty = 0L
      ),
      id = c(1, 2)
    ),
    row.names = 1:2,
    class = c('sf', 'tbl_df', 'tbl', 'data.frame'),
    sf_column = 'geometry', agr = structure(c(new = NA_integer_, id = NA_integer_),
      class = 'factor', .Label = c('constant', 'aggregate', 'identity')
    )
  )


  e2 <- structure(list(1L, 2L),
    predicate = 'equals', region.id = c('1', '2'),
    ncol = 2L, class = c('sgbp', 'list')
  )
  expect_equal(nrow(a), 2)
  expect_s3_class(a, class = 'data.frame')
  # expect_equal(sf::st_equals(a, e), e2)
})
