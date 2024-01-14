test_that('geo_estimate_down works', {
  a <- geo_estimate_down(
    from = va18sub, to = va_blocks,
    wts = va_blocks$pop, value = va18sub$G18USSDKAI
  )

  expect_equal(
    summary(a),
    structure(
      c(
        Min. = 0, `1st Qu.` = 0, Median = 4.06040880516099,
        Mean = 14.9238275102298, `3rd Qu.` = 16.0268669711954,
        Max. = 595.774925962488
      ),
      class = c('summaryDefault', 'table')
    )
  )
})

test_that('geo_estimate_up works', {
  a <- geo_estimate_up(
    from = va_blocks, to = va18sub,
    value = va_blocks$pop
  )

  expect_equal(
    summary(a),
    structure(c(
      Min. = 397, `1st Qu.` = 2300, Median = 3184.5, Mean = 3410.38888888889,
      `3rd Qu.` = 4587, Max. = 7871
    ), class = c('summaryDefault', 'table'))
  )
})

test_that('estimate_down works', {
  m <- geo_match(va_blocks, va18sub)
  a <- estimate_down(
    wts = va_blocks$pop, value = va18sub$G18USSDKAI,
    group = m
  )

  expect_equal(
    summary(a),
    structure(
      c(
        Min. = 0, `1st Qu.` = 0, Median = 4.06040880516099,
        Mean = 14.9238275102298, `3rd Qu.` = 16.0268669711954,
        Max. = 595.774925962488
      ),
      class = c('summaryDefault', 'table')
    )
  )
})

test_that('geo_estimate_up works', {
  m <- geo_match(va_blocks, va18sub)
  a <- estimate_up(value = va_blocks$pop, m)

  expect_equal(
    summary(a),
    structure(c(
      Min. = 397, `1st Qu.` = 2300, Median = 3184.5, Mean = 3410.38888888889,
      `3rd Qu.` = 4587, Max. = 7871
    ), class = c('summaryDefault', 'table'))
  )
})
