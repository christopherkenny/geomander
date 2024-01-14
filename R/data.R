#' Checkerboard
#'
#' This data set contains 64 squares in an 8x8 grid, like a checkerboard.
#'
#' @name checkerboard
#' @usage data("checkerboard")
#' @format An sf dataframe with 64 observations
#' @concept data
#' @examples
#' data('checkerboard')
NULL

#' Checkerboard Adjacency
#'
#' This data contains a zero indexed adjacency list for the checkerboard dataset.
#'
#' @name checkerboard_adj
#' @usage data("checkerboard_adj")
#' @format A list with 64 entries
#' @concept data
#' @examples
#' data('checkerboard_adj')
NULL

#' va18sub
#'
#' This data contains a 90 precinct subset of Virginia from the 2018 Senate race.
#' Contains results for Henrico County
#'
#' @name va18sub
#' @usage data("va18sub")
#' @format An sf dataframe with 90 observations
#' @concept data
#' @references
#' Voting and Election Science Team, 2019, "va_2018.zip", 2
#' 018 Precinct-Level Election Results,
#' https://doi.org/10.7910/DVN/UBKYRU/FQDLOO, Harvard Dataverse, V4
#'
#' @examples
#' data('va18sub')
NULL


#' towns
#'
#' This data contains 7 town boundaries for the towns which overlap
#' North Rockland School District in NY.
#'
#' @name towns
#' @usage data("towns")
#' @format An sf dataframe with 7 observations
#' @concept data
#' @references
#' https://www.rocklandgis.com/portal/apps/sites/#/data/items/746ec7870a0b4f46b168e07369e79a27
#'
#' @examples
#' data('towns')
NULL


#' rockland
#'
#' This data contains the blocks for Rockland County NY, with geographies simplified
#' to allow for better examples.
#'
#' It can be recreated with:
#' rockland <- create_block_table('NY', 'Rockland')
#' rockland <- rmapshaper::ms_simplify(rockland, keep_shapes = TRUE)
#'
#' @name rockland
#' @usage data("rockland")
#' @format An sf dataframe with 4764 observations
#' @concept data
#' @examples
#' data('rockland')
NULL

#' orange
#'
#' This data contains the blocks for Orange County NY, with geographies simplified
#' to allow for better examples.
#'
#' It can be recreated with:
#' orange <- create_block_table('NY', 'Orange')
#' orange <- rmapshaper::ms_simplify(orange, keep_shapes = TRUE)
#'
#' @name orange
#' @usage data("orange")
#' @format An sf dataframe with 10034 observations
#' @concept data
#' @examples
#' data('orange')
NULL

#' precincts
#'
#' This data contains the election districts (or precincts) for Rockland County
#' NY, with geographies simplified to allow for better examples.
#'
#'
#' @name precincts
#' @usage data("precincts")
#' @format An sf dataframe with 278 observations
#' @concept data
#' @references
#' https://www.rocklandgis.com/portal/apps/sites/#/data/datasets/2d91f9db816c48318848ad66eb1a18e9
#'
#' @examples
#' data('precincts')
NULL


#' nrcsd
#'
#' The data contains the North Rockland Central School District.
#'
#' @name nrcsd
#' @usage data('nrcsd')
#' @format An sf dataframe with 1 observation
#' @concept data
#' @examples
#' data('nrcsd')
NULL

#' va_blocks
#'
#' This data contains the blocks Henrico County, VA with geographies simplified
#' to allow for better examples.
#'
#' blocks87 <- create_block_table(state = 'VA', county = '087')
#' va_blocks <- rmapshaper::ms_simplify(va_blocks, keep_shapes = TRUE)
#'
#' @name va_blocks
#' @usage data("va_blocks")
#' @format An sf dataframe with 6354 observations
#' @concept data
#' @examples
#' data('va_blocks')
NULL

#' va_vtd
#'
#' This data contains the blocks for Henrico County, VA with geographies simplified
#' to allow for better examples.
#'
#' va_vtd <- tinytiger::tt_voting_districts(state = 'VA', county = '087', year = 2010)
#' va_vtd <- rmapshaper::ms_simplify(va_vtd, keep_shapes = TRUE)
#'
#' @name va_vtd
#' @usage data("va_blocks")
#' @format An sf dataframe with 93 observations
#' @concept data
#' @examples
#' data('va_blocks')
NULL
