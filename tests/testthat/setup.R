data('checkerboard')
data('checkerboard_adj')
data('va_blocks')
data('va18sub')
data('rockland')
data('nrcsd')
data('orange')

o_and_r <- rbind(orange, rockland)
o_and_r <- o_and_r %>%
  geo_filter(nrcsd) %>%
  geo_trim(nrcsd)
