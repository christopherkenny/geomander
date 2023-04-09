test_that('make planar x only works', {
  data(towns)
  x <- make_planar_pair(towns)

  expect_equal(sf::st_crs(x$x)[[1]], 'EPSG:3857')
})

test_that('make planar x == y, no planar works', {
  data(towns)
  x <- make_planar_pair(towns, towns)

  expect_equal(sf::st_crs(x$x)[[1]], 'EPSG:3857')
  expect_equal(sf::st_crs(x$y)[[1]], 'EPSG:3857')
})

test_that('make planar x != y, one planar works', {
  data(towns)
  towns2 <- sf::st_transform(towns, 2163)
  x <- make_planar_pair(towns, towns2)

  expect_equal(sf::st_crs(x$x)[[1]], 'EPSG:2163')
  expect_equal(sf::st_crs(x$y)[[1]], 'EPSG:2163')


  x <- make_planar_pair(towns2, towns)

  expect_equal(sf::st_crs(x$x)[[1]], 'EPSG:2163')
  expect_equal(sf::st_crs(x$y)[[1]], 'EPSG:2163')
})

test_that('make planar x != y, both planar works', {
  data(towns)
  towns <- sf::st_transform(towns, 3857)
  towns2 <- sf::st_transform(towns, 2163)
  x <- make_planar_pair(towns, towns2)

  expect_equal(sf::st_crs(x$x)[[1]], 'EPSG:3857')
  expect_equal(sf::st_crs(x$y)[[1]], 'EPSG:3857')
})
