test_that("split works", {
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

  
  expect_equal(nrow(a), 2)
  expect_s3_class(a, class = 'data.frame')
})
