test_that('geo_sort works', {
  a <- geo_sort(checkerboard, epsg = FALSE)$id

  e <- c(
    8L, 7L, 16L, 15L, 6L, 24L, 14L, 23L, 5L, 22L, 32L, 13L, 31L,
    21L, 30L, 4L, 40L, 12L, 39L, 29L, 20L, 38L, 3L, 48L, 11L, 28L,
    37L, 47L, 19L, 46L, 36L, 2L, 27L, 45L, 56L, 10L, 55L, 18L, 54L,
    35L, 44L, 26L, 53L, 1L, 64L, 9L, 63L, 43L, 17L, 34L, 52L, 62L,
    25L, 61L, 42L, 51L, 33L, 60L, 50L, 41L, 59L, 49L, 58L, 57L
  )
  expect_equal(a, e)
})
