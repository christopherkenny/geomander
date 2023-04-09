test_that('adjacency works', {
  actual <- adjacency(checkerboard, epsg = FALSE)

  expect_equal(lapply(actual, sort), lapply(checkerboard_adj, sort))
})

test_that('check_contiguity works', {
  expected <- structure(
    list(
      group = c(
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L
      ),
      group_number = c(
        1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
      ),
      component = c(
        1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
      )
    ),
    row.names = c(NA, -64L), class = c('tbl_df', 'tbl', 'data.frame')
  )

  actual <- check_contiguity(checkerboard_adj)

  expect_equal(actual, expected)
})


test_that('check_contiguity with groups works', {
  wv_adj <- list(
    c(9, 39), 24, c(4, 10, 53), c(4, 14, 19, 28, 36), c(2, 3, 10, 14, 36, 53),
    c(20, 22, 25, 33, 34, 48, 50), c(11, 24), c(12, 21, 40, 51),
    c(13, 16, 35, 41, 44, 46), c(0, 17, 18, 33, 39, 50), c(2, 4, 14, 19, 30),
    c(6, 26, 27), c(7, 21, 35), c(8, 16, 38, 41), c(3, 4, 10, 19),
    c(28, 40, 47, 49, 51), c(8, 13, 20, 38, 45, 46, 48), c(9, 18, 28, 31, 49, 50),
    c(9, 17, 28, 36, 39), c(3, 10, 14, 28, 29, 30, 54), c(5, 16, 34, 38, 42, 48),
    c(7, 12, 23, 35, 40, 46, 47), c(5, 25, 26, 27, 34, 42), c(21, 31, 45, 46, 47),
    c(1, 6), c(5, 22, 27, 33), c(11, 22), c(11, 22, 25),
    c(3, 15, 17, 18, 19, 36, 49, 51, 54), c(19, 30, 32, 54), c(10, 19, 29),
    c(17, 23, 45, 47, 49, 50), c(29, 51, 54), c(5, 9, 25, 50), c(5, 20, 22, 42),
    c(8, 12, 21, 46), c(3, 4, 18, 28, 39, 53), 52, c(13, 16, 20, 41, 42),
    c(0, 9, 18, 36, 53), c(7, 15, 21, 47, 51), c(8, 13, 38, 42), c(20, 22, 34, 38, 41),
    c(44, 52), c(8, 43), c(16, 23, 31, 46, 48, 50), c(8, 16, 21, 23, 35, 45),
    c(15, 21, 23, 31, 40, 49), c(5, 16, 20, 45, 50), c(15, 17, 28, 31, 47),
    c(5, 9, 17, 31, 33, 45, 48), c(7, 15, 28, 32, 40, 54), c(37, 43),
    c(2, 4, 36, 39), c(19, 28, 29, 32, 51)
  )

  wv_pl <- c(
    1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L,
    1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L,
    2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L
  )

  actual <- check_contiguity(wv_adj, wv_pl)

  expected <- structure(
    list(
      group = c(
        1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
        1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L,
        1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L,
        2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L
      ),
      group_number = c(
        1,
        2, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 2,
        2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 2, 2, 1, 1, 2, 2,
        2, 2, 2, 2, 1, 2, 1, 1, 1, 2, 1, 1
      ),
      component = c(
        1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L, 1L
      )
    ),
    class = c('tbl_df', 'tbl', 'data.frame'),
    row.names = c(NA, -55L)
  )
  
  expect_equal(actual, expected)
})
