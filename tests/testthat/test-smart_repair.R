# test data helpers

# Two overlapping rectangles: overlap strip from x=0.5 to x=1.5
make_overlap_shp <- function() {
  r1 <- sf::st_polygon(list(matrix(
    c(0, 0, 1.5, 0, 1.5, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  r2 <- sf::st_polygon(list(matrix(
    c(0.5, 0, 2, 0, 2, 1, 0.5, 1, 0.5, 0),
    ncol = 2,
    byrow = TRUE
  )))
  sf::st_sf(id = 1:2, geometry = sf::st_sfc(r1, r2))
}

# Four non-overlapping squares forming a 2x2 grid (perfect tiling)
make_perfect_shp <- function() {
  sq1 <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  sq2 <- sf::st_polygon(list(matrix(
    c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
    ncol = 2,
    byrow = TRUE
  )))
  sq3 <- sf::st_polygon(list(matrix(
    c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1),
    ncol = 2,
    byrow = TRUE
  )))
  sq4 <- sf::st_polygon(list(matrix(
    c(1, 1, 2, 1, 2, 2, 1, 2, 1, 1),
    ncol = 2,
    byrow = TRUE
  )))
  sf::st_sf(id = 1:4, geometry = sf::st_sfc(sq1, sq2, sq3, sq4))
}

# Four overlapping rectangles forming a thick frame with a small central gap.
# The gap at (0.45,0.45)-(0.55,0.55) has area 0.01, well under max_gap_frac.
make_gap_shp <- function() {
  top <- sf::st_polygon(list(matrix(
    c(0, 0.55, 1, 0.55, 1, 1, 0, 1, 0, 0.55),
    ncol = 2,
    byrow = TRUE
  )))
  bottom <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 0.45, 0, 0.45, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  left <- sf::st_polygon(list(matrix(
    c(0, 0, 0.45, 0, 0.45, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  right <- sf::st_polygon(list(matrix(
    c(0.55, 0, 1, 0, 1, 1, 0.55, 1, 0.55, 0),
    ncol = 2,
    byrow = TRUE
  )))
  sf::st_sf(id = 1:4, geometry = sf::st_sfc(top, bottom, left, right))
}

# Four NON-overlapping rectangles forming a frame with a small central gap.
# No overlaps means filling the gap can only add area, never remove it.
make_pure_gap_shp <- function() {
  top <- sf::st_polygon(list(matrix(
    c(0, 0.55, 1, 0.55, 1, 1, 0, 1, 0, 0.55),
    ncol = 2,
    byrow = TRUE
  )))
  bottom <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 0.45, 0, 0.45, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  left <- sf::st_polygon(list(matrix(
    c(0, 0.45, 0.45, 0.45, 0.45, 0.55, 0, 0.55, 0, 0.45),
    ncol = 2,
    byrow = TRUE
  )))
  right <- sf::st_polygon(list(matrix(
    c(0.55, 0.45, 1, 0.45, 1, 0.55, 0.55, 0.55, 0.55, 0.45),
    ncol = 2,
    byrow = TRUE
  )))
  sf::st_sf(id = 1:4, geometry = sf::st_sfc(top, bottom, left, right))
}

# 8 precincts in roughly a [0,10]x[0,10] space with these weirdnesses:
#
#  Overlaps:
#   2-way: P1 and P2 share a large strip [4.5,5.5]x[5.5,10]
#   3-way: P1, P2, and P7 (L-shape) all overlap at [4.5,5.5]x[7.0,9.5]
#   4-way: P1, P2, P6 (top-center), and P7 overlap at [4.5,5.5]x[8.5,9.5]
#   P5 overlaps both P3 and P4 in [3.5,4.5]x[0,4.5] and [5.5,6.5]x[0,4.5]
#   P8 is a tiny square fully contained inside P3 (containment overlap)
#
#  Gaps:
#   Central enclosed gap [4.5,5.5]x[4.5,5.5] — surrounded by 5 precincts
#   (P1 north, P2 north, P3 west, P4 east, P5 south) → 5-sub-boundary case
#
#  Other:
#   P6 extends above y=10 (precinct sticking outside overall bounding box)
#   P7 is a non-convex L-shape
#
make_convoluted_shp <- function() {
  # P1: NW block — extends too far east, creating 2-way overlap with P2
  p1 <- sf::st_polygon(list(matrix(
    c(0.0, 5.5, 5.5, 5.5, 5.5, 10.0, 0.0, 10.0, 0.0, 5.5),
    ncol = 2,
    byrow = TRUE
  )))

  # P2: NE block — extends too far west, overlapping P1 in [4.5,5.5]x[5.5,10]
  p2 <- sf::st_polygon(list(matrix(
    c(4.5, 5.5, 10.0, 5.5, 10.0, 10.0, 4.5, 10.0, 4.5, 5.5),
    ncol = 2,
    byrow = TRUE
  )))

  # P3: SW block — stops short at x=4.5 (east), leaving center-south gap
  p3 <- sf::st_polygon(list(matrix(
    c(0.0, 0.0, 4.5, 0.0, 4.5, 5.5, 0.0, 5.5, 0.0, 0.0),
    ncol = 2,
    byrow = TRUE
  )))

  # P4: SE block — stops short at x=5.5 (west), leaving center-south gap
  p4 <- sf::st_polygon(list(matrix(
    c(5.5, 0.0, 10.0, 0.0, 10.0, 5.5, 5.5, 5.5, 5.5, 0.0),
    ncol = 2,
    byrow = TRUE
  )))

  # P5: S-center strip — closes off the bottom of the center-south gap,
  # overlapping P3 in [3.5,4.5]x[0,4.5] and P4 in [5.5,6.5]x[0,4.5].
  # Together with P1-P4, this encloses the gap [4.5,5.5]x[4.5,5.5].
  p5 <- sf::st_polygon(list(matrix(
    c(3.5, 0.0, 6.5, 0.0, 6.5, 4.5, 3.5, 4.5, 3.5, 0.0),
    ncol = 2,
    byrow = TRUE
  )))

  # P6: Top-center — sticks above y=10, creating a 3-way overlap with P1 and P2
  # at [4.5,5.5]x[8.5,10], and a 4-way when combined with P7 at [4.5,5.5]x[8.5,9.5]
  p6 <- sf::st_polygon(list(matrix(
    c(3.0, 8.5, 7.0, 8.5, 7.0, 11.0, 3.0, 11.0, 3.0, 8.5),
    ncol = 2,
    byrow = TRUE
  )))

  # P7: L-shaped precinct — non-convex, overlaps P1, P2, and P6 in various zones.
  # Vertices trace a Γ-shape (bottom strip + left column):
  #   bottom strip: [3.5,7.5]x[7.0,8.5]
  #   left column:  [3.5,5.5]x[7.0,9.5]
  # 3-way: P1∩P2∩P7 = [4.5,5.5]x[7.0,9.5]
  # 4-way: P1∩P2∩P6∩P7 = [4.5,5.5]x[8.5,9.5]
  p7 <- sf::st_polygon(list(matrix(
    c(3.5, 7.0, 7.5, 7.0, 7.5, 8.5, 5.5, 8.5, 5.5, 9.5, 3.5, 9.5, 3.5, 7.0),
    ncol = 2,
    byrow = TRUE
  )))

  # P8: Tiny square fully contained inside P3 — tests containment overlap handling
  p8 <- sf::st_polygon(list(matrix(
    c(0.8, 0.8, 2.0, 0.8, 2.0, 2.0, 0.8, 2.0, 0.8, 0.8),
    ncol = 2,
    byrow = TRUE
  )))

  sf::st_sf(
    id = 1:8,
    precinct = c('NW', 'NE', 'SW', 'SE', 'S-center', 'top', 'L-shape', 'tiny'),
    geometry = sf::st_sfc(p1, p2, p3, p4, p5, p6, p7, p8)
  )
}


# input validation
test_that('smart_repair validates inputs and parameter ranges', {
  shp <- make_overlap_shp()

  expect_error(smart_repair(data.frame(x = 1)), 'sf')
  expect_error(
    smart_repair(sf::st_sf(
      id = 1,
      geometry = sf::st_sfc(sf::st_point(c(0, 0)))
    )),
    'polygon'
  )
  expect_error(smart_repair(shp, max_gap_frac = -1), 'max_gap_frac')
  expect_error(smart_repair(shp, min_component_frac = 2), 'min_component_frac')
})


#  overlap resolution

test_that('smart_repair resolves overlaps and preserves total area', {
  shp <- make_overlap_shp()
  result <- smart_repair(shp, epsg = FALSE)

  # Output structure
  expect_s3_class(result, 'sf')
  expect_equal(nrow(result), nrow(shp))
  expect_true('id' %in% names(result))
  expect_true(all(
    as.character(sf::st_geometry_type(result)) %in%
      c('POLYGON', 'MULTIPOLYGON')
  ))

  # Overlap eliminated
  g <- sf::st_geometry(result)
  expect_true(
    as.numeric(sf::st_area(sf::st_intersection(g[[1]], g[[2]]))) < 1e-10
  )

  # Union of the two rectangles covers [0,2] x [0,1] = area 2
  expect_equal(as.numeric(sum(sf::st_area(result))), 2.0, tolerance = 1e-6)
})


#  perfect tiling (no change needed) -

test_that('smart_repair leaves a perfect tiling unchanged', {
  shp <- make_perfect_shp()
  result <- smart_repair(shp, epsg = FALSE)

  expect_equal(nrow(result), 4)
  expect_equal(
    sort(as.numeric(sf::st_area(result))),
    sort(as.numeric(sf::st_area(shp))),
    tolerance = 1e-6
  )
})


# gap filling

test_that('smart_repair fills enclosed gaps (overlapping frame)', {
  shp <- make_gap_shp()
  result <- smart_repair(shp, epsg = FALSE)

  expect_equal(nrow(result), 4)
  expect_true(all(sf::st_is_valid(result)))
  # Gap filled: total area should cover the full 1x1 bounding box
  expect_equal(sum(as.numeric(sf::st_area(result))), 1.0, tolerance = 1e-6)
})

test_that('smart_repair fills pure gap without shrinking any unit and splits among neighbors', {
  shp <- make_pure_gap_shp()
  original_areas <- as.numeric(sf::st_area(shp))
  result <- smart_repair(shp, epsg = FALSE)
  repaired_areas <- as.numeric(sf::st_area(result))

  expect_equal(nrow(result), 4)
  expect_true(all(sf::st_is_valid(result)))

  # No unit should lose area (gap filling only adds)
  shrink_ok <- repaired_areas >= original_areas - 1e-10
  expect_true(
    all(shrink_ok),
    label = paste(
      sprintf(
        'Unit %d shrank (was %.6f, now %.6f)',
        which(!shrink_ok),
        original_areas[!shrink_ok],
        repaired_areas[!shrink_ok]
      ),
      collapse = '\n'
    )
  )

  # Full 1x1 bounding box covered
  expect_equal(sum(repaired_areas), 1.0, tolerance = 1e-6)

  # Gap (0.01) is adjacent to all 4 units equally; at least 2 should gain area
  # (smart split, not all-to-one assignment)
  gained <- sum(repaired_areas > original_areas + 1e-10)
  expect_true(
    gained >= 2,
    label = sprintf('Expected >= 2 units to gain area, got %d', gained)
  )
})


#  convoluted multi-pathology example

test_that('smart_repair handles convoluted input: structure and validity', {
  shp <- make_convoluted_shp()
  result <- smart_repair(shp, epsg = FALSE)

  expect_s3_class(result, 'sf')
  expect_equal(nrow(result), nrow(shp))
  expect_true(all(sf::st_is_valid(result)))
  expect_true('precinct' %in% names(result))
})

test_that('smart_repair handles convoluted input: overlaps resolved, gap filled, area preserved', {
  shp <- make_convoluted_shp()
  result <- smart_repair(shp, epsg = FALSE)
  g <- sf::st_geometry(result)
  n <- length(g)

  # All pairwise overlaps eliminated
  overlap_pairs <- utils::combn(n, 2L)
  overlap_areas <- apply(overlap_pairs, 2L, function(idx) {
    as.numeric(sf::st_area(sf::st_intersection(g[[idx[1L]]], g[[idx[2L]]])))
  })
  overlap_ok <- overlap_areas < 1e-8
  expect_true(
    all(overlap_ok),
    label = paste(
      sprintf(
        'Overlap between units %d and %d: area = %.2e',
        overlap_pairs[1L, !overlap_ok],
        overlap_pairs[2L, !overlap_ok],
        overlap_areas[!overlap_ok]
      ),
      collapse = '\n'
    )
  )

  # Enclosed center gap [4.5,5.5]x[4.5,5.5] fully covered
  gap_poly <- sf::st_sfc(sf::st_polygon(list(matrix(
    c(4.5, 4.5, 5.5, 4.5, 5.5, 5.5, 4.5, 5.5, 4.5, 4.5),
    ncol = 2,
    byrow = TRUE
  ))))
  uncovered_geom <- sf::st_difference(gap_poly, sf::st_union(g))
  uncovered <- sum(as.numeric(sf::st_area(uncovered_geom)))
  expect_true(
    uncovered < 1e-8,
    label = sprintf('Center gap not fully covered; remaining = %.2e', uncovered)
  )

  # Total repaired area >= union of inputs (gap filled, not lost)
  union_area <- as.numeric(sf::st_area(sf::st_union(sf::st_geometry(shp))))
  result_area <- sum(as.numeric(sf::st_area(result)))
  expect_true(
    result_area >= union_area - 1e-6,
    label = sprintf(
      'Result area %.6f < union area %.6f',
      result_area,
      union_area
    )
  )
})
# Toy 4x4 precinct grid with jagged edges (overlaps and gaps between neighbors).
# Each cell (i,j) nominally occupies [0.5*i, 0.5*(i+1)] x [0.5*j, 0.5*(j+1)],
# with edges perturbed by up to 1/24 in the perpendicular direction.
make_toy_precincts <- function() {
  set.seed(2023)
  polys <- vector('list', 16L)
  idx <- 1L
  for (i in 0:3) {
    for (j in 0:3) {
      bottom <- cbind(0.5 * i + 0.1 * (0:5), 0.5 * j + (runif(6) - 0.5) / 12)
      right <- cbind(
        0.5 * (i + 1) + (runif(5) - 0.5) / 12,
        0.5 * j + 0.1 * (1:5)
      )
      top <- cbind(
        0.5 * (i + 1) - 0.1 * (1:5),
        0.5 * (j + 1) + (runif(5) - 0.5) / 12
      )
      left <- cbind(
        0.5 * i + (runif(4) - 0.5) / 12,
        0.5 * (j + 1) - 0.1 * (1:4)
      )
      coords <- rbind(bottom, right, top, left)
      coords <- rbind(coords, coords[1L, , drop = FALSE])
      polys[[idx]] <- sf::st_polygon(list(coords))
      idx <- idx + 1L
    }
  }
  sf::st_sf(id = seq_len(16L), geometry = sf::st_sfc(polys))
}

# Four 1x1 unit squares tiling [0,2]x[0,2] (the toy county grid).
make_toy_counties <- function() {
  mk <- function(x0, x1, y0, y1) {
    sf::st_polygon(list(matrix(
      c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0),
      ncol = 2,
      byrow = TRUE
    )))
  }
  sf::st_sf(
    id = 1:4,
    geometry = sf::st_sfc(
      mk(0, 1, 0, 1),
      mk(1, 2, 0, 1),
      mk(0, 1, 1, 2),
      mk(1, 2, 1, 2)
    )
  )
}

# Minimum shared linear-boundary length across all rook-adjacent pairs.
# Used to verify short-rook-to-queen conversion.
min_rook_length <- function(shp) {
  g <- sf::st_geometry(shp)
  n <- length(g)
  min_len <- Inf
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      inter <- suppressWarnings(sf::st_intersection(g[[i]], g[[j]]))
      if (!sf::st_is_empty(inter)) {
        len <- as.numeric(sf::st_length(inter))
        if (len > 0 && len < min_len) min_len <- len
      }
    }
  }
  min_len
}


test_that('smart_repair basic output is a clean sf (no overlaps, valid geometries)', {
  precincts <- make_toy_precincts()
  result <- smart_repair(precincts, epsg = FALSE)

  expect_s3_class(result, 'sf')
  expect_equal(nrow(result), nrow(precincts))
  expect_true(all(sf::st_is_valid(result)))

  # Equivalent to Python doctor(): sum of areas equals union area (no residual overlaps)
  total_area <- sum(as.numeric(sf::st_area(result)))
  union_area <- as.numeric(sf::st_area(sf::st_union(result)))
  expect_equal(total_area, union_area, tolerance = 1e-6)
})

test_that('smart_repair nest_within_regions keeps each precinct inside its county', {
  precincts <- make_toy_precincts()
  counties <- make_toy_counties()

  # Assign each precinct to the county containing its centroid (mirrors maup.assign)
  cent_within <- sf::st_within(
    sf::st_centroid(sf::st_geometry(precincts)),
    sf::st_geometry(counties),
    sparse = TRUE
  )
  precincts$county_id <- vapply(cent_within, `[[`, integer(1L), 1L)

  result <- smart_repair(
    precincts,
    regions = counties,
    region_col = 'county_id',
    epsg = FALSE
  )

  diff_areas <- vapply(seq_len(nrow(result)), function(p) {
    c_idx <- precincts$county_id[p]
    as.numeric(sf::st_area(
      sf::st_difference(
        sf::st_geometry(result)[[p]],
        sf::st_geometry(counties)[[c_idx]]
      )
    ))
  }, double(1L))
  diff_ok <- diff_areas < 1e-8
  expect_true(
    all(diff_ok),
    label = paste(
      sprintf(
        'Precinct %d extends outside county %d by area %.2e',
        which(!diff_ok),
        precincts$county_id[!diff_ok],
        diff_areas[!diff_ok]
      ),
      collapse = '\n'
    )
  )
})

test_that('smart_repair rook_threshold converts short rook adjacencies to queen', {
  precincts <- make_toy_precincts()
  basic_result <- smart_repair(precincts, epsg = FALSE)
  srtq_result <- smart_repair(precincts, rook_threshold = 0.05, epsg = FALSE)

  # After basic repair, short rook adjacencies still exist
  expect_true(min_rook_length(basic_result) < 0.05)

  # After rook_threshold = 0.05, all remaining shared boundaries are >= 0.05
  expect_true(min_rook_length(srtq_result) >= 0.05)
})
