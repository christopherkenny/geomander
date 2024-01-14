test_that('geo_filter works', {
  nr <- geo_filter(rockland, nrcsd)

  expect_equal(nrow(nr), 722)
})


test_that('geo_trim works', {
  nr <- geo_filter(rockland, nrcsd)
  nr <- geo_trim(nr, nrcsd)
  expect_equal(nrow(nr), 689)
})
