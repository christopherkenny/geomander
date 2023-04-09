test_that('seam_rip works', {
  adj <- adjacency(o_and_r)

  a <- seam_rip(adj, o_and_r, 'county', c('071', '087')) %>%
    unlist() %>%
    length()
  e <- 3772

  expect_equal(a, e)
})

test_that('seam_geom works', {
  adj <- adjacency(o_and_r)

  a <- seam_geom(adj, o_and_r, 'county', c('071', '087')) %>% nrow()
  e <- 24

  expect_equal(a, e)
})

test_that('seam_adj works', {
  adj <- adjacency(o_and_r)

  a <- seam_adj(adj, o_and_r, 'county', c('071', '087')) %>%
    unlist() %>%
    length()
  e <- 32

  expect_equal(a, e)
})

test_that('seam_sew works', {
  a <- seam_sew(o_and_r, 'county', c('071', '087')) %>%
    unlist() %>%
    length()
  e <- 98

  expect_equal(a, e)
})
