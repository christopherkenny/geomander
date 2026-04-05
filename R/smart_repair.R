#' Smart Repair of Noisy Polygonal Tilings
#'
#' Repairs gaps and overlaps in a set of polygons (an approximate tiling) to
#' produce a clean tiling that preserves adjacency relations as faithfully as
#' possible. Unlike simple "quick repair" methods that assign each gap or
#' overlap to a single polygon (often creating spurious adjacencies), this
#' algorithm subdivides gaps among neighboring units to reproduce the true
#' adjacency structure.
#'
#' The algorithm follows Clelland (2025), "Mend the gap: A smart repair
#' algorithm for noisy polygonal tilings," and proceeds in four main phases:
#'
#' 1. **Construct refined tiling** — compute the fully noded union of all unit
#'    boundaries and polygonize to obtain a set of non-overlapping pieces, each
#'    tagged with its *overlap order* (i.e., how many input units contain it).
#' 2. **Assign overlaps** — assign order-1 pieces to their unique parent unit,
#'    then assign higher-order overlaps in increasing order. Connectivity of
#'    each unit is prioritized; ties are broken by largest shared perimeter.
#' 3. **Close gaps** — subdivide order-0 pieces (gaps) among adjacent units.
#'    Gaps are partitioned by the number of *sub-boundaries* (connected
#'    components of the gap boundary intersecting distinct units):
#'    - 1 sub-boundary: assign the entire gap to the sole adjacent unit.
#'    - 2 sub-boundaries: split along the shortest interior path between
#'      sub-boundary endpoints (optimizing convexity).
#'    - 3 sub-boundaries: convexify, then partition from the incenter of the
#'      convex hull (or use a fallback shortest-path split when the incenter
#'      lies outside the gap).
#'    - 4+ sub-boundaries: convexify, then iteratively find the closest
#'      strongly mutually visible pair of non-adjacent sub-boundaries, split
#'      the gap along shortest-path "diagonals", and recurse on the remaining
#'      sub-gaps.
#' 4. **Clean up** — reassign small orphaned connected components of
#'    disconnected units to the adjacent unit sharing the largest perimeter.
#'
#' Two optional post-processing steps are available:
#' - **Region-aware nesting**: if `regions` is provided, the repair is
#'   performed so that repaired units nest cleanly into the region polygons
#'   (e.g., precincts into counties).
#' - **Small rook-to-queen conversion**: rook adjacencies shorter than
#'   `rook_threshold` are converted to queen (point) adjacencies by excising a
#'   small disk and replacing it with pie-piece wedges.
#'
#' @param shp An `sf` data frame of polygon geometries representing the
#'   approximate tiling to be repaired.
#' @param regions An optional `sf` data frame of polygon geometries representing
#'   larger administrative units (e.g., counties) into which the repaired
#'   polygons should nest. Default is `NULL` (no nesting constraint).
#' @param region_col A character string naming the column in `shp` that
#'   identifies which region each unit belongs to. Required when `regions` is
#'   provided. Default is `NULL`.
#' @param max_gap_frac Numeric between 0 and 1. Gaps whose area exceeds this
#'   fraction of the area of the largest adjacent unit are left unfilled.
#'   Default is `0.1`.
#' @param min_component_frac Numeric between 0 and 1. During cleanup, orphaned
#'   connected components whose area is less than this fraction of the largest
#'   component are reassigned. Default is `0.0001`.
#' @param rook_threshold Positive numeric length threshold for the optional
#'   rook-to-queen adjacency conversion. Rook adjacencies shorter than this
#'   value (in the units of the projected CRS) are converted to queen
#'   adjacencies. Set to `0` or `NULL` to skip. Default is `NULL`.
#' @templateVar epsg TRUE
#' @template template
#'
#' @return An `sf` data frame with the same rows and attributes as `shp`, but
#'   with repaired polygon geometries that form a clean tiling of the region.
#'   If any units remain disconnected after repair, a message is issued
#'   identifying them.
#'
#' @export
#'
#' @concept fix
#'
#' @references
#' Clelland, Jeanne N. "Mend the gap: A smart repair algorithm for noisy
#' polygonal tilings." 2025.
#'
#' @examples
#' library(sf)
#'
#' # Basic repair
#' repaired <- smart_repair(precincts)
#'
#' # Region-aware repair (nest precincts into counties)
#' repaired <- smart_repair(precincts, regions = counties,
#'                          region_col = "county_fips")
#'
#' # With rook-to-queen conversion (threshold in CRS units, e.g., meters)
#' repaired <- smart_repair(precincts, rook_threshold = 30)
sr_compute_snap_magnitude <- function(shp, regions = NULL, snap_precision = 9L) {
  geom_bbox <- sf::st_bbox(shp)
  widths <- c(geom_bbox$xmax - geom_bbox$xmin, geom_bbox$ymax - geom_bbox$ymin)

  if (!is.null(regions)) {
    region_bbox <- sf::st_bbox(regions)
    widths <- c(
      widths,
      region_bbox$xmax - region_bbox$xmin,
      region_bbox$ymax - region_bbox$ymin
    )
  }

  largest_bound <- max(as.numeric(widths), na.rm = TRUE)
  if (!is.finite(largest_bound) || largest_bound <= 0) {
    return(NULL)
  }

  floor(log10(largest_bound)) - snap_precision
}


sr_snap_sfc_to_grid <- function(geom, snap_magnitude) {
  if (is.null(snap_magnitude)) {
    return(geom)
  }

  grid_size <- 10^snap_magnitude
  geom_crs <- sf::st_crs(geom)
  snapped <- geos::geos_set_precision(
    geos::as_geos_geometry(sf::st_set_crs(geom, NA)),
    grid_size,
    preserve_topology = TRUE
  )
  snapped_sfc <- sf::st_as_sfc(snapped)
  sf::st_set_crs(snapped_sfc, geom_crs)
}


sr_snap_sf_to_grid <- function(shp, snap_magnitude) {
  sf::st_geometry(shp) <- sr_snap_sfc_to_grid(sf::st_geometry(shp), snap_magnitude)
  shp
}


sr_extract_polygonal_sfc <- function(geom) {
  geom_types <- as.character(sf::st_geometry_type(geom, by_geometry = TRUE))
  out <- geom

  collection_idx <- which(geom_types == 'GEOMETRYCOLLECTION')
  if (length(collection_idx) > 0L) {
    out[collection_idx] <- sf::st_collection_extract(geom[collection_idx], type = 'POLYGON')
  }

  out
}

smart_repair <- function(
  shp,
  regions = NULL,
  region_col = NULL,
  max_gap_frac = 0.1,
  min_component_frac = 1e-04,
  rook_threshold = NULL,
  epsg = 3857
) {
  # --- Input validation -------------------------------------------------
  if (!inherits(shp, 'sf')) {
    cli::cli_abort('{.arg shp} must be an {.cls sf} data frame.')
  }
  geom_types <- as.character(unique(sf::st_geometry_type(shp)))
  if (!all(geom_types %in% c('POLYGON', 'MULTIPOLYGON'))) {
    cli::cli_abort('{.arg shp} must contain polygon geometries.')
  }

  region_aware <- !is.null(regions)
  if (region_aware) {
    if (!inherits(regions, 'sf')) {
      cli::cli_abort('{.arg regions} must be an {.cls sf} data frame.')
    }
    if (is.null(region_col) || !region_col %in% names(shp)) {
      cli::cli_abort(
        '{.arg region_col} must name a column in {.arg shp} when {.arg regions} is provided.'
      )
    }

    region_geom_check <- sf::st_geometry(regions)
    region_total <- sum(as.numeric(sf::st_area(region_geom_check)))
    region_union_area <- as.numeric(
      sf::st_area(sf::st_union(region_geom_check))
    )
    if (region_total > region_union_area * (1 + 1e-6)) {
      cli::cli_warn(
        '{.arg regions} appear to have overlapping geometries. \\
        Results may be unreliable; provide non-overlapping regions for best results.'
      )
    }
  }

  if (max_gap_frac < 0 || max_gap_frac > 1) {
    cli::cli_abort('{.arg max_gap_frac} must be between 0 and 1.')
  }
  if (min_component_frac < 0 || min_component_frac > 1) {
    cli::cli_abort('{.arg min_component_frac} must be between 0 and 1.')
  }

  n_units <- nrow(shp)
  original_crs <- sf::st_crs(shp)
  snap_magnitude <- NULL

  # --- Planarize --------------------------------------------------------
  planar <- make_planar_pair(shp, regions, epsg = epsg)
  shp <- planar$x
  regions <- planar$y

  # --- Validate and repair geometries -----------------------------------
  invalid_units <- which(!sf::st_is_valid(shp))
  if (length(invalid_units) > 0L) {
    cli::cli_warn(
      '{length(invalid_units)} unit{?s} {?has/have} invalid geometr{?y/ies}; \\
      repairing with {.fn sf::st_make_valid} before processing.'
    )
    fixed <- sf::st_make_valid(sf::st_geometry(shp))
    # make_valid can produce GEOMETRYCOLLECTION; extract only polygon parts
    sf::st_geometry(shp) <- sr_extract_polygonal_sfc(fixed)
  }
  if (region_aware) {
    invalid_regions <- which(!sf::st_is_valid(regions))
    if (length(invalid_regions) > 0L) {
      cli::cli_warn(
        '{length(invalid_regions)} region{?s} {?has/have} invalid geometr{?y/ies}; \\
        repairing with {.fn sf::st_make_valid} before processing.'
      )
      fixed_r <- sf::st_make_valid(sf::st_geometry(regions))
      sf::st_geometry(regions) <- sr_extract_polygonal_sfc(fixed_r)
    }
  }

  # --- Snap to grid -----------------------------------------------------
  snap_magnitude <- sr_compute_snap_magnitude(shp, regions, snap_precision = 9L)
  if (!is.null(snap_magnitude)) {
    shp <- sr_snap_sf_to_grid(shp, snap_magnitude)
    if (region_aware) {
      regions <- sr_snap_sf_to_grid(regions, snap_magnitude)
    }
  }

  # --- Convert to geos once ---------------------------------------------
  # Strip CRS before entering geos-land: geos does not use coordinate reference
  # systems, and wk (used internally by geos) will error when mixing a
  # NULL-CRS empty polygon with a CRS-carrying geometry during [<- assignments.
  # The correct CRS is re-attached to the sfc at the end of the function.
  unit_geom <- geos::as_geos_geometry(sf::st_set_crs(sf::st_geometry(shp), NA))
  region_geom <- if (region_aware) {
    geos::as_geos_geometry(sf::st_set_crs(sf::st_geometry(regions), NA))
  }

  # Save original component counts for disconnection cleanup (Phase 4)
  original_n_comps <- geos::geos_num_geometries(unit_geom)

  # PHASE 0 (optional): Assign units to regions
  unit_region <- sr_assign_units_to_regions(unit_geom, region_geom)

  # PHASE 1: Construct refined tiling
  refined <- sr_construct_refined_tiling(unit_geom, region_geom, unit_region, snap_magnitude)

  pieces <- refined$pieces
  piece_parents <- refined$piece_parents
  overlap_order <- refined$overlap_order
  piece_region <- refined$piece_region

  # PHASE 2: Assign overlaps
  piece_assignment <- sr_assign_overlaps(
    pieces,
    piece_parents,
    overlap_order,
    n_units
  )

  # PHASE 3: Close gaps
  repaired <- sr_build_repaired_geom(pieces, piece_assignment, n_units)
  repaired <- sr_normalize_polygon_geom(repaired)
  repaired <- sr_close_gaps(
    pieces,
    overlap_order,
    repaired,
    n_units,
    max_gap_frac,
    piece_region = piece_region,
    unit_region = unit_region
  )
  repaired <- sr_rebuild_repaired_geom(repaired)

  # PHASE 4: Clean up disconnected units
  repaired <- sr_normalize_polygon_geom(repaired)
  repaired <- sr_cleanup_disconnected(
    repaired, min_component_frac, original_n_comps, unit_geom
  )

  # PHASE 5 (optional): Small rook-to-queen adjacency conversion
  repaired <- sr_normalize_polygon_geom(repaired)
  repaired <- sr_rook_to_queen(repaired, rook_threshold)
  repaired <- sr_rebuild_repaired_geom(repaired)

  # --- Reassemble output ------------------------------------------------
  repaired_sfc <- sf::st_as_sfc(repaired)
  sf::st_crs(repaired_sfc) <- sf::st_crs(shp)
  sf::st_geometry(shp) <- repaired_sfc

  # Transform back to the original CRS if planarized
  if (!is.na(original_crs) && !identical(original_crs, sf::st_crs(shp))) {
    shp <- sf::st_transform(shp, original_crs)
  }

  shp
}


# --- Phase 0 helper: assign units to regions ----------------------------

#' Assign each unit polygon to the region it overlaps most
#'
#' @param unit_geom `geos_geometry` vector of unit polygons.
#' @param region_geom `geos_geometry` vector of region polygons, or `NULL`.
#'
#' @return An integer vector of length `length(unit_geom)` mapping each unit to
#'   an index in `region_geom`, or `NULL` when `region_geom` is `NULL`.
#'
#' @noRd
sr_assign_units_to_regions <- function(unit_geom, region_geom) {
  if (is.null(region_geom)) {
    return(NULL)
  }

  unit_region <- largest_intersection_geos(unit_geom, region_geom)

  n_na <- sum(is.na(unit_region))
  if (n_na > 0) {
    cli::cli_warn(
      '{n_na} unit{?s} {?does/do} not intersect any region and will be dropped.'
    )
  }

  unit_region
}


# --- Phase 1 helpers: construct refined tiling --------------------------

#' Construct the refined tiling from unit (and optionally region) geometries
#'
#' This performs the three sub-steps of Phase 1:
#'
#' 1a. Build the fully noded union of all polygon boundaries.
#' 1b. Polygonize the noded skeleton into non-overlapping pieces.
#' 1c. Tag each piece with its parent unit indices and overlap order.
#'
#' @param unit_geom `geos_geometry` vector of unit polygons.
#' @param region_geom `geos_geometry` vector of region polygons, or `NULL`.
#' @param unit_region Integer vector mapping units to region indices (output of
#'   `sr_assign_units_to_regions()`), or `NULL`.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{pieces}{A `geos_geometry` vector of polygon pieces.}
#'     \item{piece_parents}{A list of integer vectors, one per piece, giving the
#'       indices (into `unit_geom`) of the original units that contain the
#'       piece.}
#'     \item{overlap_order}{An integer vector giving the overlap order of each
#'       piece (0 = gap, 1 = unique, 2+ = overlap).}
#'   }
#'
#' @noRd
sr_construct_refined_tiling <- function(unit_geom, region_geom, unit_region, snap_magnitude = NULL) {
  # -- 1a: fully noded union of boundaries --------------------------------
  noded <- sr_compute_noded_union(unit_geom, region_geom, snap_magnitude)

  # -- 1b: polygonize to obtain pieces ------------------------------------
  pieces <- sr_polygonize_skeleton(noded)

  if (length(pieces) == 0L) {
    cli::cli_abort('Polygonization produced no pieces. Check input geometries.')
  }

  # -- 1c: tag each piece with parent units and overlap order -------------
  sr_tag_pieces(pieces, unit_geom, region_geom, unit_region)
}


#' Build the fully noded union of all polygon boundaries
#'
#' Extracts the boundary of every input polygon, unions them into a single
#' geometry, and nodes the result so that every point where two edges cross
#' becomes a vertex.
#'
#' @param unit_geom `geos_geometry` vector of unit polygons.
#' @param region_geom `geos_geometry` vector of region polygons, or `NULL`.
#'
#' @return A length-1 `geos_geometry` representing the noded 1-complex.
#'
#' @noRd
sr_compute_noded_union <- function(unit_geom, region_geom, snap_magnitude = NULL) {
  unit_bounds <- geos::geos_boundary(unit_geom)

  if (!is.null(region_geom)) {
    region_bounds <- geos::geos_boundary(region_geom)
    all_bounds <- c(unit_bounds, region_bounds)
  } else {
    all_bounds <- unit_bounds
  }

  if (!is.null(snap_magnitude)) {
    all_bounds <- geos::geos_set_precision(
      all_bounds,
      10^(snap_magnitude - 1L),
      preserve_topology = TRUE
    )
  }

  # Collect into a single geometry, union, then node so that every crossing
  # point becomes a vertex.
  collected <- geos::geos_make_collection(all_bounds)
  skeleton <- geos::geos_unary_union(collected)
  noded <- geos::geos_node(skeleton)

  noded
}


#' Polygonize a noded line skeleton into non-overlapping pieces
#'
#' @param noded A length-1 `geos_geometry` (noded MULTILINESTRING) produced by
#'   `sr_compute_noded_union()`.
#'
#' @return A `geos_geometry` vector of individual POLYGON geometries.
#'
#' @noRd
sr_polygonize_skeleton <- function(noded) {
  collection <- geos::geos_polygonize(noded)
  n_pieces <- geos::geos_num_geometries(collection)

  if (n_pieces == 0L) {
    return(geos::as_geos_geometry(character(0L)))
  }

  geos::geos_geometry_n(collection, seq_len(n_pieces))
}


#' Tag pieces with parent unit indices and overlap order
#'
#' For each piece produced by polygonization, determine which original unit
#' polygons contain it. In region-aware mode, a unit is only considered a
#' parent of a piece when the piece and the unit belong to the same region;
#' pieces that fall outside all regions are dropped.
#'
#' @param pieces `geos_geometry` vector of polygon pieces.
#' @param unit_geom `geos_geometry` vector of unit polygons.
#' @param region_geom `geos_geometry` vector of region polygons, or `NULL`.
#' @param unit_region integer vector mapping units to region indices, or `NULL`.
#'
#' @return A list with `pieces`, `piece_parents`, `overlap_order`, and
#'   `piece_region` (see
#'   `sr_construct_refined_tiling()` for details).
#'
#' @noRd
sr_tag_pieces <- function(pieces, unit_geom, region_geom, unit_region) {
  n_pieces <- length(pieces)
  region_aware <- !is.null(region_geom) && !is.null(unit_region)

  # Representative point inside each piece (guaranteed to be interior)
  rep_pts <- geos::geos_point_on_surface(pieces)

  # If region-aware, also determine which region each piece belongs to
  piece_region <- NULL
  if (region_aware) {
    piece_region <- largest_intersection_geos(rep_pts, region_geom)
  }

  unit_tree <- geos::geos_strtree(unit_geom)

  # For each rep point, find which units' bounding boxes it may fall in
  candidates <- geos::geos_strtree_query(unit_tree, rep_pts)
  candidate_keys <- vapply(candidates, function(idx) {
    if (length(idx) == 0L) {
      return("")
    }
    paste(idx, collapse = ",")
  }, character(1L))
  candidate_groups <- split(seq_len(n_pieces), candidate_keys)

  # For each piece, refine candidates to true containment
  piece_parents <- vector('list', n_pieces)
  for (group_name in names(candidate_groups)) {
    piece_idxs <- candidate_groups[[group_name]]
    if (length(piece_idxs) == 0L) {
      next
    }
    cands <- candidates[[piece_idxs[1L]]]
    if (length(cands) == 0L) {
      for (piece_idx in piece_idxs) {
        piece_parents[[piece_idx]] <- integer(0L)
      }
      next
    }

    # Batch containment checks for pieces sharing the same candidate units.
    within_map <- geos::geos_within_matrix(rep_pts[piece_idxs], unit_geom[cands])
    for (offset in seq_along(piece_idxs)) {
      parents <- cands[within_map[[offset]]]

      # In region-aware mode, keep only parents assigned to the same region
      piece_idx <- piece_idxs[offset]
      if (region_aware && !is.na(piece_region[piece_idx])) {
        parents <- parents[unit_region[parents] == piece_region[piece_idx]]
      }

      piece_parents[[piece_idx]] <- as.integer(parents)
    }
  }

  overlap_order <- lengths(piece_parents)

  # In region-aware mode, drop pieces outside all regions
  if (region_aware) {
    keep <- !is.na(piece_region)
    pieces <- pieces[keep]
    piece_parents <- piece_parents[keep]
    overlap_order <- overlap_order[keep]
    piece_region <- piece_region[keep]
  }

  list(
    pieces = pieces,
    piece_parents = piece_parents,
    overlap_order = as.integer(overlap_order),
    piece_region = piece_region
  )
}


# --- Phase 2 helpers: assign overlaps -----------------------------------

#' Assign overlap pieces to units
#'
#' Order-1 pieces go to their unique parent. Higher-order pieces are assigned
#' in increasing order, prioritizing connectivity, then largest shared
#' perimeter.
#'
#' @param pieces `geos_geometry` vector of polygon pieces.
#' @param piece_parents List of integer vectors (parent unit indices per piece).
#' @param overlap_order Integer vector of overlap orders.
#' @param n_units Number of original units.
#'
#' @return An integer vector of length `length(pieces)` with each element being
#'   the unit index the piece is assigned to, or `NA` for gaps (order 0).
#'
#' @noRd
sr_assign_overlaps <- function(pieces, piece_parents, overlap_order, n_units) {
  n_pieces <- length(pieces)
  piece_assignment <- rep(NA_integer_, n_pieces)
  orphaned_overlaps <- integer(0L)

  # Step 1: assign order-1 pieces to their unique parent
  order1 <- which(overlap_order == 1L)
  for (i in order1) {
    piece_assignment[i] <- piece_parents[[i]][1L]
  }

  max_order <- max(overlap_order)
  if (max_order < 2L) {
    return(piece_assignment)
  }

  # Step 2: process higher-order overlaps
  for (d in seq.int(2L, max_order)) {
    order_d <- which(overlap_order == d & is.na(piece_assignment))
    if (length(order_d) == 0L) {
      next
    }

    # Build current repaired geometry to test connectivity
    repaired <- sr_build_repaired_geom(pieces, piece_assignment, n_units)
    n_comps <- geos::geos_num_geometries(repaired)
    disconnected <- which(n_comps > 1L & !geos::geos_is_empty(repaired))

    # 2a. Connectivity-first: assign overlaps that reconnect disconnected units
    for (i in order_d) {
      if (!is.na(piece_assignment[i])) {
        next
      }
      parents <- piece_parents[[i]]
      disc_parents <- intersect(parents, disconnected)
      if (length(disc_parents) == 0L) {
        next
      }

      for (k in disc_parents) {
        candidate <- geos::geos_union(repaired[k], pieces[i])
        new_n <- geos::geos_num_geometries(candidate)
        if (new_n < n_comps[k]) {
          piece_assignment[i] <- k
          repaired[k] <- sr_normalize_polygon_geom(candidate)
          n_comps[k] <- geos::geos_num_geometries(repaired[k])
          if (new_n <= 1L) {
            disconnected <- setdiff(disconnected, k)
          }
          break
        }
      }
    }

    # 2b. Assign remaining by largest shared perimeter
    #     Tiebreaker: assign to the parent with the smallest current area
    #     (load-balancing prevents all overlaps going to one unit).
    #     Update repaired geometry after each assignment.
    #     Pieces that can't find a home (no parent with shared perimeter > 0)
    #     are saved as orphans and retried after the full overlap tower.
    remaining_d <- order_d[is.na(piece_assignment[order_d])]
    if (length(remaining_d) == 0L) {
      next
    }

    for (i in remaining_d) {
      parents <- piece_parents[[i]]
      if (length(parents) == 0L) {
        next
      }
      if (length(parents) == 1L) {
        piece_assignment[i] <- parents[1L]
        repaired[parents[1L]] <- sr_normalize_polygon_geom(
          geos::geos_union(repaired[parents[1L]], pieces[i])
        )
        next
      }

      piece_bound <- geos::geos_boundary(pieces[i])
      shared_perim <- vapply(
        parents,
        function(k) {
          if (geos::geos_is_empty(repaired[k])) {
            return(0)
          }
          inter <- geos::geos_intersection(
            piece_bound,
            geos::geos_boundary(repaired[k])
          )
          geos::geos_length(inter)
        },
        double(1)
      )

      max_perim <- max(shared_perim)
      if (max_perim <= 0) {
        orphaned_overlaps <- c(orphaned_overlaps, i)
        next
      }
      tied <- which(abs(shared_perim - max_perim) < 1e-10 * max_perim)
      if (length(tied) == 1L) {
        best <- parents[tied]
      } else {
        # Tiebreak: assign to parent with smallest current repaired area
        tied_areas <- geos::geos_area(repaired[parents[tied]])
        best <- parents[tied[which.min(tied_areas)]]
      }

      piece_assignment[i] <- best
      repaired[best] <- sr_normalize_polygon_geom(geos::geos_union(repaired[best], pieces[i]))
    }
  }

  # Retry orphaned overlaps now that the full overlap tower has been assigned
  if (length(orphaned_overlaps) > 0L) {
    repaired <- sr_build_repaired_geom(pieces, piece_assignment, n_units)
    for (i in orphaned_overlaps) {
      if (!is.na(piece_assignment[i])) next
      parents <- piece_parents[[i]]
      if (length(parents) == 0L) next

      piece_bound <- geos::geos_boundary(pieces[i])
      shared_perim <- vapply(
        parents,
        function(k) {
          if (geos::geos_is_empty(repaired[k])) return(0)
          geos::geos_length(
            geos::geos_intersection(piece_bound, geos::geos_boundary(repaired[k]))
          )
        },
        double(1)
      )

      max_perim <- max(shared_perim)
      if (max_perim <= 0) next
      tied <- which(abs(shared_perim - max_perim) < 1e-10 * max_perim)
      if (length(tied) == 1L) {
        best <- parents[tied]
      } else {
        tied_areas <- geos::geos_area(repaired[parents[tied]])
        best <- parents[tied[which.min(tied_areas)]]
      }

      piece_assignment[i] <- best
      repaired[best] <- sr_normalize_polygon_geom(
        geos::geos_union(repaired[best], pieces[i])
      )
    }
  }

  piece_assignment
}


#' Build repaired geometries by unioning assigned pieces per unit
#'
#' @param pieces `geos_geometry` vector of polygon pieces.
#' @param piece_assignment Integer vector mapping pieces to unit indices.
#' @param n_units Number of units.
#'
#' @return A `geos_geometry` vector of length `n_units`.
#'
#' @noRd
sr_build_repaired_geom <- function(pieces, piece_assignment, n_units) {
  empty <- geos::as_geos_geometry("POLYGON EMPTY")
  repaired <- rep(empty, n_units)

  for (k in seq_len(n_units)) {
    assigned <- which(piece_assignment == k)
    if (length(assigned) == 0L) {
      next
    }
    if (length(assigned) == 1L) {
      repaired[k] <- pieces[assigned]
    } else {
      coll <- geos::geos_make_collection(pieces[assigned])
      repaired[k] <- geos::geos_unary_union(coll)
    }
  }

  repaired
}


#' Normalize a geos_geometry vector to contain only POLYGON/MULTIPOLYGON
#'
#' `geos_union` and `geos_unary_union` can occasionally return a
#' `GEOMETRYCOLLECTION` for degenerate inputs (e.g. near-collinear vertices
#' that produce slivers). Subsequent calls to `geos_boundary` on a
#' `GEOMETRYCOLLECTION` throw an `IllegalArgumentException`. This helper
#' extracts only the polygon parts from any collection-type result, leaving
#' true polygon geometries unchanged.
#'
#' @param repaired `geos_geometry` vector.
#'
#' @return A `geos_geometry` vector of the same length with all
#'   GEOMETRYCOLLECTION entries replaced by their polygon parts.
#'
#' @noRd
sr_normalize_polygon_geom <- function(repaired) {
  types    <- geos::geos_type(repaired)
  coll_idx <- which(grepl('COLLECTION', types, ignore.case = TRUE))
  if (length(coll_idx) == 0L) {
    return(repaired)
  }
  for (idx in coll_idx) {
    n_parts <- geos::geos_num_geometries(repaired[idx])
    if (n_parts == 0L) {
      repaired[idx] <- geos::as_geos_geometry('POLYGON EMPTY')
      next
    }
    parts     <- geos::geos_geometry_n(repaired[idx], seq_len(n_parts))
    poly_mask <- grepl('POLYGON', geos::geos_type(parts), ignore.case = TRUE)
    poly_parts <- parts[poly_mask]
    repaired[idx] <- if (length(poly_parts) == 0L) {
      geos::as_geos_geometry('POLYGON EMPTY')
    } else if (length(poly_parts) == 1L) {
      poly_parts[[1L]]
    } else {
      geos::geos_unary_union(geos::geos_make_collection(poly_parts))
    }
  }
  repaired
}


# --- Phase 3 helpers: close gaps ----------------------------------------

# Utility: polygon ring coordinates without closing duplicate
sr_coords <- function(geom) {
  coords <- wk::wk_coords(geom)
  n <- nrow(coords)
  if (n > 1L &&
      abs(coords$x[1L] - coords$x[n]) < 1e-12 &&
      abs(coords$y[1L] - coords$y[n]) < 1e-12) {
    coords <- coords[-n, , drop = FALSE]
  }
  coords
}

# Utility: incenter of a triangle from a 3-row data.frame with columns x, y
sr_incenter <- function(v) {
  xa <- v[1L, "x"]; ya <- v[1L, "y"]
  xb <- v[2L, "x"]; yb <- v[2L, "y"]
  xc <- v[3L, "x"]; yc <- v[3L, "y"]
  a <- sqrt((xb - xc)^2 + (yb - yc)^2)  # BC (opposite A)
  b <- sqrt((xa - xc)^2 + (ya - yc)^2)  # CA (opposite B)
  c <- sqrt((xa - xb)^2 + (ya - yb)^2)  # AB (opposite C)
  s <- a + b + c
  if (s <= 0) return(data.frame(x = xa, y = ya))
  data.frame(x = (a * xa + b * xb + c * xc) / s,
             y = (a * ya + b * yb + c * yc) / s)
}

# Utility: convert (x, y) data.frame path to geos LINESTRING, or NULL if < 2 rows
sr_path_to_linestring <- function(path_df) {
  if (is.null(path_df) || nrow(path_df) < 2L) return(NULL)
  pts <- paste(sprintf("%.15g %.15g", path_df$x, path_df$y), collapse = ", ")
  geos::as_geos_geometry(paste0("LINESTRING (", pts, ")"))
}

# Utility: build a geos POINT from a 1-row data.frame (x, y)
sr_xy_to_point <- function(xy) {
  geos::as_geos_geometry(sprintf("POINT (%.15g %.15g)", xy$x, xy$y))
}


sr_xy_equal <- function(a_xy, b_xy, tol = 1e-8) {
  abs(a_xy$x[1L] - b_xy$x[1L]) < tol &&
    abs(a_xy$y[1L] - b_xy$y[1L]) < tol
}


sr_xy_key <- function(xy, digits = 12L) {
  sprintf(
    paste0("%.", digits, "f_%.", digits, "f"),
    xy$x[1L],
    xy$y[1L]
  )
}


sr_xy_keys <- function(xy_df, digits = 12L) {
  sprintf(
    paste0("%.", digits, "f_%.", digits, "f"),
    xy_df$x,
    xy_df$y
  )
}


sr_xy_from_key <- function(key, coord_map) {
  idx <- match(key, coord_map$key)
  data.frame(
    x = coord_map$x[idx],
    y = coord_map$y[idx]
  )
}


sr_segment_geom <- function(start_xy, end_xy) {
  geos::as_geos_geometry(sprintf(
    "LINESTRING (%.15g %.15g, %.15g %.15g)",
    start_xy$x,
    start_xy$y,
    end_xy$x,
    end_xy$y
  ))
}


sr_segment_in_polygon <- function(poly_geom, start_xy, end_xy) {
  isTRUE(geos::geos_covers(poly_geom, sr_segment_geom(start_xy, end_xy)))
}


sr_point_on_segment <- function(point_xy, start_xy, end_xy, tol = 1e-8) {
  dx_seg <- end_xy$x[1L] - start_xy$x[1L]
  dy_seg <- end_xy$y[1L] - start_xy$y[1L]
  dx_pt <- point_xy$x[1L] - start_xy$x[1L]
  dy_pt <- point_xy$y[1L] - start_xy$y[1L]

  cross <- dx_seg * dy_pt - dy_seg * dx_pt
  if (abs(cross) > tol) {
    return(FALSE)
  }

  dot <- dx_pt * dx_seg + dy_pt * dy_seg
  if (dot < -tol) {
    return(FALSE)
  }

  seg_len_sq <- dx_seg * dx_seg + dy_seg * dy_seg
  dot <= seg_len_sq + tol
}


sr_polygon_orientation_area <- function(poly_geom) {
  coords <- sr_coords(poly_geom)
  next_idx <- c(seq.int(2L, nrow(coords)), 1L)
  sum(coords$x * coords$y[next_idx] - coords$x[next_idx] * coords$y) / 2
}


sr_orient_polygon_ccw <- function(poly_geom) {
  if (sr_polygon_orientation_area(poly_geom) < 0) {
    geos::geos_reverse(poly_geom)
  } else {
    poly_geom
  }
}


sr_geometry_list <- function(geom) {
  n_geom <- geos::geos_num_geometries(geom)
  if (n_geom <= 1L) {
    return(list(geom))
  }
  lapply(seq_len(n_geom), function(i) geos::geos_geometry_n(geom, i))
}


sr_polygon_parts <- function(poly_geom) {
  geom_type <- toupper(geos::geos_type(poly_geom))
  if (geom_type == "POLYGON") {
    return(list(poly_geom))
  }
  if (geom_type == "MULTIPOLYGON" || geom_type == "GEOMETRYCOLLECTION") {
    parts <- sr_geometry_list(poly_geom)
    return(Filter(function(g) toupper(geos::geos_type(g)) == "POLYGON", parts))
  }
  list()
}


sr_triangles_in_polygon <- function(poly_geom) {
  tri_coll <- geos::geos_constrained_delaunay_triangles(poly_geom)
  Filter(function(g) toupper(geos::geos_type(g)) == "POLYGON", sr_geometry_list(tri_coll))
}


sr_prepare_shortest_path_context <- function(poly_geom) {
  poly_oriented <- sr_orient_polygon_ccw(poly_geom)
  boundary_pts <- sr_coords(poly_oriented)[, c("x", "y"), drop = FALSE]
  boundary_keys <- sr_xy_keys(boundary_pts)
  triangles <- sr_triangles_in_polygon(poly_oriented)
  triangle_keys <- lapply(triangles, function(tri) sr_xy_keys(sr_triangle_vertices(tri)))

  list(
    poly_oriented = poly_oriented,
    boundary_pts = boundary_pts,
    boundary_keys = boundary_keys,
    triangles = triangles,
    triangle_keys = triangle_keys
  )
}


sr_triangle_vertices <- function(triangle_geom) {
  sr_coords(triangle_geom)[, c("x", "y"), drop = FALSE]
}


sr_triangle_contains_key <- function(triangle_keys, key) {
  key %in% triangle_keys
}


sr_shared_edge <- function(geom1, geom2) {
  inter <- geos::geos_intersection(geom1, geom2)
  !geos::geos_is_empty(inter) &&
    grepl("LINE", toupper(geos::geos_type(inter))) &&
    geos::geos_length(inter) > 0
}


sr_union_geometry_list <- function(geom_list) {
  if (length(geom_list) == 0L) {
    return(geos::as_geos_geometry("POLYGON EMPTY"))
  }
  if (length(geom_list) == 1L) {
    return(geom_list[[1L]])
  }
  geos::geos_unary_union(geos::geos_make_collection(do.call(c, geom_list)))
}


sr_shortest_path_visibility_graph <- function(poly_geom, start_xy, end_xy) {
  eps <- 1e-8

  if (sr_segment_in_polygon(poly_geom, start_xy, end_xy)) {
    return(data.frame(x = c(start_xy$x, end_xy$x),
                      y = c(start_xy$y, end_xy$y)))
  }

  coords <- sr_coords(poly_geom)
  verts <- data.frame(x = coords$x, y = coords$y)

  has_vertex <- function(xy) {
    any(abs(verts$x - xy$x) < eps & abs(verts$y - xy$y) < eps)
  }
  if (!has_vertex(start_xy)) {
    verts <- rbind(verts, data.frame(x = start_xy$x, y = start_xy$y))
  }
  if (!has_vertex(end_xy)) {
    verts <- rbind(verts, data.frame(x = end_xy$x, y = end_xy$y))
  }

  start_idx <- which(abs(verts$x - start_xy$x) < eps &
                     abs(verts$y - start_xy$y) < eps)[1L]
  end_idx <- which(abs(verts$x - end_xy$x) < eps &
                   abs(verts$y - end_xy$y) < eps)[1L]
  n_v <- nrow(verts)

  if (start_idx == end_idx) {
    return(data.frame(x = start_xy$x, y = start_xy$y))
  }

  vis_dist <- matrix(Inf, n_v, n_v)
  diag(vis_dist) <- 0
  for (i in seq_len(n_v - 1L)) {
    for (j in seq.int(i + 1L, n_v)) {
      if (sr_segment_in_polygon(
        poly_geom,
        data.frame(x = verts$x[i], y = verts$y[i]),
        data.frame(x = verts$x[j], y = verts$y[j])
      )) {
        d <- sqrt((verts$x[i] - verts$x[j])^2 + (verts$y[i] - verts$y[j])^2)
        vis_dist[i, j] <- d
        vis_dist[j, i] <- d
      }
    }
  }

  visited <- logical(n_v)
  dist_v <- rep(Inf, n_v)
  prev_v <- rep(NA_integer_, n_v)
  dist_v[start_idx] <- 0
  for (iter in seq_len(n_v)) {
    u <- which.min(ifelse(visited, Inf, dist_v))
    if (is.infinite(dist_v[u])) break
    if (u == end_idx) break
    visited[u] <- TRUE
    for (v in seq_len(n_v)) {
      if (!visited[v] && is.finite(vis_dist[u, v])) {
        nd <- dist_v[u] + vis_dist[u, v]
        if (nd < dist_v[v]) {
          dist_v[v] <- nd
          prev_v[v] <- u
        }
      }
    }
  }

  path <- integer(0L)
  cur <- end_idx
  while (!is.na(cur)) {
    path <- c(cur, path)
    cur <- prev_v[cur]
  }
  if (length(path) == 0L || path[1L] != start_idx) {
    return(data.frame(x = c(start_xy$x, end_xy$x), y = c(start_xy$y, end_xy$y)))
  }
  data.frame(x = verts$x[path], y = verts$y[path])
}


#' Find the shortest path between two points within a simply-connected polygon
#'
#' Builds a visibility graph on the polygon's vertices (extended with start/end
#' if they are not already vertices, e.g. an interior incenter) and runs
#' Dijkstra to find the shortest path.
#'
#' @param poly_geom A length-1 `geos_geometry` POLYGON (simply connected).
#' @param start_xy  A 1-row data.frame with columns `x` and `y`.
#' @param end_xy    A 1-row data.frame with columns `x` and `y`.
#'
#' @return A data.frame with columns `x` and `y` giving path vertices
#'   (start to end inclusive).
#'
#' @noRd
sr_shortest_path_in_polygon <- function(poly_geom, start_xy, end_xy, path_context = NULL) {
  if (sr_segment_in_polygon(poly_geom, start_xy, end_xy)) {
    return(data.frame(x = c(start_xy$x, end_xy$x),
                      y = c(start_xy$y, end_xy$y)))
  }

  if (is.null(path_context)) {
    path_context <- sr_prepare_shortest_path_context(poly_geom)
  }

  poly_oriented <- path_context$poly_oriented
  boundary_pts <- path_context$boundary_pts
  boundary_keys <- path_context$boundary_keys
  start_key <- sr_xy_key(start_xy)
  end_key <- sr_xy_key(end_xy)
  if (!(start_key %in% boundary_keys) || !(end_key %in% boundary_keys)) {
    return(sr_shortest_path_visibility_graph(poly_geom, start_xy, end_xy))
  }

  if (start_key == end_key) {
    return(data.frame(x = start_xy$x, y = start_xy$y))
  }

  start_idx <- match(start_key, boundary_keys)
  end_idx <- match(end_key, boundary_keys)
  n_boundary <- nrow(boundary_pts)

  if (start_idx < end_idx) {
    right_path <- boundary_pts[start_idx:end_idx, , drop = FALSE]
    left_path <- boundary_pts[c(end_idx:n_boundary, seq_len(start_idx)), , drop = FALSE]
    left_path <- left_path[nrow(left_path):1L, , drop = FALSE]
  } else {
    right_path <- boundary_pts[c(start_idx:n_boundary, seq_len(end_idx)), , drop = FALSE]
    left_path <- boundary_pts[end_idx:start_idx, , drop = FALSE]
    left_path <- left_path[nrow(left_path):1L, , drop = FALSE]
  }

  right_inner_keys <- if (nrow(right_path) > 2L) sr_xy_keys(right_path[2:(nrow(right_path) - 1L), , drop = FALSE]) else character(0L)
  left_inner_keys <- if (nrow(left_path) > 2L) sr_xy_keys(left_path[2:(nrow(left_path) - 1L), , drop = FALSE]) else character(0L)

  triangles <- path_context$triangles
  triangle_keys <- path_context$triangle_keys
  if (length(triangles) == 0L) {
    return(sr_shortest_path_visibility_graph(poly_geom, start_xy, end_xy))
  }

  if (length(right_inner_keys) > 0L && length(left_inner_keys) > 0L) {
    keep_triangles <- Map(function(tri, tri_keys) {
        if (any(tri_keys %in% right_inner_keys) && any(tri_keys %in% left_inner_keys)) {
          list(triangle = tri, triangle_keys = tri_keys)
        } else {
          NULL
        }
      }, triangles, triangle_keys)
    keep_triangles <- Filter(Negate(is.null), keep_triangles)
    triangles <- lapply(keep_triangles, `[[`, "triangle")
    triangle_keys <- lapply(keep_triangles, `[[`, "triangle_keys")
  }
  if (length(triangles) == 0L) {
    triangles <- path_context$triangles
    triangle_keys <- path_context$triangle_keys
  }

  all_vertices <- unique(rbind(
    boundary_pts,
    data.frame(x = start_xy$x, y = start_xy$y),
    data.frame(x = end_xy$x, y = end_xy$y)
  ))
  coord_map <- transform(all_vertices, key = sr_xy_keys(all_vertices))

  initial_idx <- which(vapply(triangle_keys, sr_triangle_contains_key, logical(1L), key = start_key))
  if (length(initial_idx) == 0L) {
    return(sr_shortest_path_visibility_graph(poly_geom, start_xy, end_xy))
  }

  ordered_triangles <- list(triangles[[initial_idx[1L]]])
  if (length(triangles) > 1L) {
    remaining <- triangles[-initial_idx[1L]]
    while (length(remaining) > 0L) {
      lead_tri <- ordered_triangles[[length(ordered_triangles)]]
      next_idx <- which(vapply(remaining, sr_shared_edge, logical(1L), geom1 = lead_tri))
      if (length(next_idx) == 0L) {
        break
      }
      ordered_triangles[[length(ordered_triangles) + 1L]] <- remaining[[next_idx[1L]]]
      remaining <- remaining[-next_idx[1L]]
    }
  }

  polygon_simplified <- sr_union_geometry_list(ordered_triangles)

  ordered_path_keys <- start_key
  right_simplified_keys <- start_key
  left_simplified_keys <- start_key
  right_path_keys <- sr_xy_keys(right_path)
  left_path_keys <- sr_xy_keys(left_path)

  for (tri in ordered_triangles) {
    tri_keys <- sr_xy_keys(sr_triangle_vertices(tri))
    tri_new <- tri_keys[!tri_keys %in% ordered_path_keys]
    if (length(tri_new) == 0L) {
      next
    }
    ordered_path_keys <- c(ordered_path_keys, tri_new)
    right_simplified_keys <- c(right_simplified_keys, tri_new[tri_new %in% right_path_keys])
    left_simplified_keys <- c(left_simplified_keys, tri_new[tri_new %in% left_path_keys])
  }

  if (length(left_simplified_keys) < 2L || length(right_simplified_keys) < 2L || length(ordered_path_keys) < 3L) {
    return(sr_shortest_path_visibility_graph(poly_geom, start_xy, end_xy))
  }

  found_path_keys <- start_key
  left_funnel <- c(left_simplified_keys[1L], left_simplified_keys[2L])
  right_funnel <- c(right_simplified_keys[1L], right_simplified_keys[2L])
  ordered_path_keys <- ordered_path_keys[-seq_len(min(3L, length(ordered_path_keys)))]

  for (point_key in ordered_path_keys) {
    apex_key <- found_path_keys[length(found_path_keys)]
    apex_xy <- sr_xy_from_key(apex_key, coord_map)
    point_xy <- sr_xy_from_key(point_key, coord_map)

    if (point_key %in% left_simplified_keys) {
      this_funnel <- left_funnel
      other_funnel <- right_funnel
      reflex_sign <- 1
      update_left <- TRUE
    } else if (point_key %in% right_simplified_keys) {
      this_funnel <- right_funnel
      other_funnel <- left_funnel
      reflex_sign <- -1
      update_left <- FALSE
    } else {
      next
    }

    if (sr_segment_in_polygon(polygon_simplified, apex_xy, point_xy)) {
      new_funnel <- apex_key
      if (length(this_funnel) > 1L) {
        for (i in seq.int(2L, length(this_funnel))) {
          funnel_xy <- sr_xy_from_key(this_funnel[i], coord_map)
          if (sr_point_on_segment(funnel_xy, apex_xy, point_xy)) {
            new_funnel <- c(new_funnel, this_funnel[i])
          }
        }
      }
      this_funnel <- c(new_funnel, point_key)
    } else {
      visible_idx <- integer(0L)
      if (length(this_funnel) > 1L) {
        for (i in seq.int(2L, length(this_funnel))) {
          funnel_xy <- sr_xy_from_key(this_funnel[i], coord_map)
          if (sr_segment_in_polygon(polygon_simplified, funnel_xy, point_xy)) {
            visible_idx <- i
            break
          }
        }
      }
      if (length(visible_idx) == 0L) {
        return(sr_shortest_path_visibility_graph(poly_geom, start_xy, end_xy))
      }

      prev_xy <- sr_xy_from_key(this_funnel[visible_idx - 1L], coord_map)
      seen_xy <- sr_xy_from_key(this_funnel[visible_idx], coord_map)
      vec1_x <- seen_xy$x[1L] - prev_xy$x[1L]
      vec1_y <- seen_xy$y[1L] - prev_xy$y[1L]
      vec2_x <- point_xy$x[1L] - seen_xy$x[1L]
      vec2_y <- point_xy$y[1L] - seen_xy$y[1L]
      cross_prod <- vec1_x * vec2_y - vec1_y * vec2_x

      if (cross_prod * reflex_sign >= 0) {
        new_funnel <- this_funnel[seq_len(visible_idx)]
        if (visible_idx < length(this_funnel)) {
          for (i in seq.int(visible_idx + 1L, length(this_funnel))) {
            this_seen_xy <- sr_xy_from_key(this_funnel[i], coord_map)
            if (sr_point_on_segment(this_seen_xy, seen_xy, point_xy)) {
              new_funnel <- c(new_funnel, this_funnel[i])
            }
          }
        }
        this_funnel <- c(new_funnel, point_key)
      } else {
        other_seen_idx <- integer(0L)
        if (length(other_funnel) > 1L) {
          for (i in seq.int(2L, length(other_funnel))) {
            funnel_xy <- sr_xy_from_key(other_funnel[i], coord_map)
            if (sr_segment_in_polygon(polygon_simplified, funnel_xy, point_xy)) {
              other_seen_idx <- i
              break
            }
          }
        }
        if (length(other_seen_idx) == 0L) {
          return(sr_shortest_path_visibility_graph(poly_geom, start_xy, end_xy))
        }

        found_path_keys <- c(found_path_keys, other_funnel[seq.int(2L, other_seen_idx)])
        apex_key <- other_funnel[other_seen_idx]
        apex_xy <- sr_xy_from_key(apex_key, coord_map)
        other_start_idx <- other_seen_idx

        if (other_seen_idx < length(other_funnel)) {
          for (i in seq.int(other_seen_idx + 1L, length(other_funnel))) {
            funnel_xy <- sr_xy_from_key(other_funnel[i], coord_map)
            if (sr_point_on_segment(funnel_xy, apex_xy, point_xy)) {
              found_path_keys <- c(found_path_keys, other_funnel[i])
              apex_key <- other_funnel[i]
              apex_xy <- sr_xy_from_key(apex_key, coord_map)
              other_start_idx <- i
            }
          }
        }

        this_funnel <- c(apex_key, point_key)
        other_funnel <- other_funnel[seq.int(other_start_idx, length(other_funnel))]
      }
    }

    if (update_left) {
      left_funnel <- this_funnel
      right_funnel <- other_funnel
    } else {
      right_funnel <- this_funnel
      left_funnel <- other_funnel
    }
  }

  found_path_keys <- c(found_path_keys, left_funnel[-1L])
  path_rows <- lapply(found_path_keys, sr_xy_from_key, coord_map = coord_map)
  do.call(rbind, path_rows)
}


#' Identify sub-boundary arcs of a gap polygon
#'
#' Returns a list of `list(unit, arc)` entries: for each adjacent repaired unit
#' the shared arc(s), plus any exterior portions tagged with `unit = NA`.
#'
#' @param gap_geom      A length-1 `geos_geometry` POLYGON.
#' @param repaired      `geos_geometry` vector.
#' @param adjacent_units Integer vector of adjacent unit indices.
#'
#' @noRd
sr_construct_gap_boundaries <- function(gap_geom, repaired, adjacent_units) {
  gap_bound <- geos::geos_boundary(gap_geom)
  result    <- list()
  covered   <- list()
  gap_coords <- sr_coords(gap_geom)
  span <- max(gap_coords$x) - min(gap_coords$x) + max(gap_coords$y) - min(gap_coords$y)
  min_arc_length <- max(span * 1e-8, 1e-12)

  for (k in adjacent_units) {
    if (geos::geos_is_empty(repaired[k])) next
    shared <- geos::geos_intersection(gap_bound, geos::geos_boundary(repaired[k]))
    if (geos::geos_is_empty(shared) || geos::geos_length(shared) <= min_arc_length) next

    if (grepl("COLLECTION|MULTI", geos::geos_type(shared), ignore.case = TRUE)) {
      n_p   <- geos::geos_num_geometries(shared)
      parts <- geos::geos_geometry_n(shared, seq_len(n_p))
      for (p in seq_along(parts)) {
        if (geos::geos_length(parts[p]) > min_arc_length) {
          result  <- c(result,  list(list(unit = k,   arc = parts[p])))
          covered <- c(covered, list(parts[p]))
        }
      }
    } else {
      result  <- c(result,  list(list(unit = k,   arc = shared)))
      covered <- c(covered, list(shared))
    }
  }

  if (length(covered) > 0L) {
    cov  <- geos::geos_unary_union(geos::geos_make_collection(do.call(c, covered)))
    ext  <- geos::geos_difference(gap_bound, cov)
    if (!geos::geos_is_empty(ext) && geos::geos_length(ext) > min_arc_length) {
      if (grepl("MULTI|COLLECTION", geos::geos_type(ext), ignore.case = TRUE)) {
        n_e   <- geos::geos_num_geometries(ext)
        parts <- geos::geos_geometry_n(ext, seq_len(n_e))
        for (p in seq_along(parts))
          if (geos::geos_length(parts[p]) > min_arc_length)
            result <- c(result, list(list(unit = NA_integer_, arc = parts[p])))
      } else {
        result <- c(result, list(list(unit = NA_integer_, arc = ext)))
      }
    }
  }

  result
}


#' Order gap sub-boundaries in cyclic order around the gap ring
#'
#' @noRd
sr_order_boundaries <- function(gap_geom, boundaries) {
  gap_c <- sr_coords(gap_geom)
  n_b   <- length(boundaries)
  span  <- max(gap_c$x) - min(gap_c$x) + max(gap_c$y) - min(gap_c$y) + 1
  eps   <- span * 1e-6

  pos <- integer(n_b)
  for (i in seq_len(n_b)) {
    ac <- wk::wk_coords(boundaries[[i]]$arc)
    dists <- sqrt((gap_c$x - ac$x[1L])^2 + (gap_c$y - ac$y[1L])^2)
    m  <- which.min(dists)
    pos[i] <- if (dists[m] < eps) m else NA_integer_
  }

  valid <- !is.na(pos)
  if (!any(valid)) return(boundaries)
  boundaries[c(which(valid)[order(pos[valid])], which(!valid))]
}


sr_merge_boundary_run <- function(boundary_run) {
  if (length(boundary_run) == 1L) {
    return(boundary_run[[1L]])
  }

  merged_arc <- tryCatch(
    geos::geos_line_merge(
      geos::geos_unary_union(
        geos::geos_make_collection(do.call(c, lapply(boundary_run, `[[`, "arc")))
      )
    ),
    error = function(e) NULL
  )

  if (is.null(merged_arc) || geos::geos_is_empty(merged_arc)) {
    return(boundary_run[[1L]])
  }

  if (grepl("MULTI|COLLECTION", geos::geos_type(merged_arc), ignore.case = TRUE)) {
    n_parts <- geos::geos_num_geometries(merged_arc)
    parts <- geos::geos_geometry_n(merged_arc, seq_len(n_parts))
    parts <- parts[geos::geos_length(parts) > 0]
    if (length(parts) != 1L) {
      return(boundary_run[[1L]])
    }
    merged_arc <- parts[1L]
  }

  list(unit = boundary_run[[1L]]$unit, arc = merged_arc)
}


sr_merge_adjacent_boundaries <- function(boundaries) {
  if (length(boundaries) <= 1L) {
    return(boundaries)
  }

  merged <- list()
  run <- list(boundaries[[1L]])
  for (i in seq.int(2L, length(boundaries))) {
    same_unit <- identical(boundaries[[i]]$unit, run[[length(run)]]$unit)
    if (same_unit) {
      run[[length(run) + 1L]] <- boundaries[[i]]
    } else {
      merged[[length(merged) + 1L]] <- sr_merge_boundary_run(run)
      run <- list(boundaries[[i]])
    }
  }
  merged[[length(merged) + 1L]] <- sr_merge_boundary_run(run)

  if (length(merged) > 1L && identical(merged[[1L]]$unit, merged[[length(merged)]]$unit)) {
    wrap_run <- list(merged[[length(merged)]], merged[[1L]])
    merged <- c(list(sr_merge_boundary_run(wrap_run)), merged[2:(length(merged) - 1L)])
  }

  merged
}


sr_gap_adjacency <- function(gap_geom, repaired) {
  repaired_tree <- geos::geos_strtree(repaired)
  candidates <- geos::geos_strtree_query(repaired_tree, gap_geom)[[1L]]
  if (length(candidates) == 0L) {
    return(NULL)
  }

  gap_bound <- geos::geos_boundary(gap_geom)
  shared_perims <- vapply(candidates, function(k) {
    if (geos::geos_is_empty(repaired[k])) {
      return(0)
    }
    geos::geos_length(
      geos::geos_intersection(gap_bound, geos::geos_boundary(repaired[k]))
    )
  }, double(1L))

  adjacent_mask <- shared_perims > 0
  if (!any(adjacent_mask)) {
    return(NULL)
  }

  list(
    candidates = candidates[adjacent_mask],
    shared_perims = shared_perims[adjacent_mask]
  )
}


sr_convexify_gaps <- function(gaps_queue, repaired, unit_region = NULL) {
  region_aware <- length(gaps_queue) > 0L &&
    is.list(gaps_queue[[1L]]) &&
    all(c("geom", "region") %in% names(gaps_queue[[1L]]))
  completed_gaps <- vector("list", 0L)

  while (length(gaps_queue) > 0L) {
    gap_item <- gaps_queue[[1L]]
    gaps_queue <- gaps_queue[-1L]
    if (region_aware) {
      gap_geom <- gap_item$geom
      gap_region <- gap_item$region
    } else {
      gap_geom <- gap_item
      gap_region <- NULL
    }

    if (geos::geos_is_empty(gap_geom) || geos::geos_num_interior_rings(gap_geom) > 0L) {
      next
    }

    adjacency <- sr_gap_adjacency(gap_geom, repaired)
    if (is.null(adjacency)) {
      completed_gaps[[length(completed_gaps) + 1L]] <- if (region_aware) {
        list(geom = gap_geom, region = gap_region)
      } else {
        gap_geom
      }
      next
    }

    if (region_aware && !is.null(unit_region) && !is.na(gap_region)) {
      keep <- unit_region[adjacency$candidates] == gap_region
      keep[is.na(keep)] <- FALSE
      adjacency$candidates <- adjacency$candidates[keep]
      adjacency$shared_perims <- adjacency$shared_perims[keep]
      if (length(adjacency$candidates) == 0L) {
        completed_gaps[[length(completed_gaps) + 1L]] <- list(
          geom = gap_geom,
          region = gap_region
        )
        next
      }
    }

    adjacent_units <- adjacency$candidates
    bounds <- sr_construct_gap_boundaries(gap_geom, repaired, adjacent_units)
    target_ids <- vapply(bounds, function(bound) {
      if (is.na(bound$unit)) -1L else as.integer(bound$unit)
    }, integer(1L))
    non_ext_targets <- unique(target_ids[target_ids != -1L])

    if (length(non_ext_targets) == 0L) {
      completed_gaps[[length(completed_gaps) + 1L]] <- if (region_aware) {
        list(geom = gap_geom, region = gap_region)
      } else {
        gap_geom
      }
      next
    }

    if (length(non_ext_targets) == 1L) {
      target_unit <- non_ext_targets[1L]
      repaired[target_unit] <- sr_normalize_polygon_geom(
        geos::geos_union(repaired[target_unit], gap_geom)
      )
      next
    }

    target_counts <- table(target_ids[target_ids != -1L])
    repeated_targets <- target_counts[target_counts > 1L]
    has_repeated_target <- length(repeated_targets) > 0L

    gap_in_progress <- gap_geom
    for (bound in bounds) {
      if (is.na(bound$unit) || geos::geos_is_empty(gap_in_progress)) {
        next
      }

      arc_coords <- wk::wk_coords(bound$arc)
      n_arc <- nrow(arc_coords)
      if (n_arc < 2L) {
        next
      }

      start_xy <- data.frame(x = arc_coords$x[1L], y = arc_coords$y[1L])
      end_xy <- data.frame(x = arc_coords$x[n_arc], y = arc_coords$y[n_arc])
      path_df <- tryCatch(
        sr_shortest_path_in_polygon(gap_in_progress, start_xy, end_xy),
        error = function(e) NULL
      )
      if (is.null(path_df) || nrow(path_df) < 2L) {
        next
      }

      path_line <- sr_path_to_linestring(path_df)
      if (is.null(path_line)) {
        next
      }

      add_boundary <- tryCatch(
        geos::geos_node(geos::geos_unary_union(
          geos::geos_make_collection(c(bound$arc, path_line))
        )),
        error = function(e) NULL
      )
      partition_boundary <- tryCatch(
        geos::geos_node(geos::geos_unary_union(
          geos::geos_make_collection(c(geos::geos_boundary(gap_in_progress), path_line))
        )),
        error = function(e) NULL
      )
      if (is.null(add_boundary) || is.null(partition_boundary)) {
        next
      }

      add_polys <- sr_polygon_parts(geos::geos_polygonize(add_boundary))
      partition_polys <- sr_polygon_parts(geos::geos_polygonize(partition_boundary))
      if (length(add_polys) == 0L) {
        next
      }

      for (poly_to_add in add_polys) {
        repaired[bound$unit] <- sr_normalize_polygon_geom(
          geos::geos_union(repaired[bound$unit], poly_to_add)
        )
        partition_polys <- Filter(function(poly_part) {
          rep_pt <- geos::geos_point_on_surface(poly_part)
          !isTRUE(geos::geos_covers(poly_to_add, rep_pt))
        }, partition_polys)
      }

      gap_in_progress <- sr_normalize_polygon_geom(sr_union_geometry_list(partition_polys))
    }

    if (geos::geos_is_empty(gap_in_progress)) {
      next
    }

    new_gaps <- sr_polygon_parts(gap_in_progress)
    for (new_gap in new_gaps) {
      if (!has_repeated_target) {
        completed_gaps[[length(completed_gaps) + 1L]] <- if (region_aware) {
          list(geom = new_gap, region = gap_region)
        } else {
          new_gap
        }
        next
      }

      new_adjacency <- sr_gap_adjacency(new_gap, repaired)
      if (is.null(new_adjacency)) {
        completed_gaps[[length(completed_gaps) + 1L]] <- if (region_aware) {
          list(geom = new_gap, region = gap_region)
        } else {
          new_gap
        }
        next
      }

      new_bounds <- sr_construct_gap_boundaries(new_gap, repaired, new_adjacency$candidates)
      new_target_ids <- vapply(new_bounds, function(bound) {
        if (is.na(bound$unit)) -1L else as.integer(bound$unit)
      }, integer(1L))
      new_target_counts <- table(new_target_ids[new_target_ids != -1L])

      reprocess_gap <- any(vapply(names(repeated_targets), function(target_name) {
        target_key <- as.integer(target_name)
        target_count <- if (target_name %in% names(new_target_counts)) new_target_counts[[target_name]] else 0L
        target_count > 0L && target_count < repeated_targets[[target_name]]
      }, logical(1L)))

      if (reprocess_gap) {
        gaps_queue[[length(gaps_queue) + 1L]] <- if (region_aware) {
          list(geom = new_gap, region = gap_region)
        } else {
          new_gap
        }
      } else {
        completed_gaps[[length(completed_gaps) + 1L]] <- if (region_aware) {
          list(geom = new_gap, region = gap_region)
        } else {
          new_gap
        }
      }
    }
  }

  list(repaired = repaired, gaps_queue = completed_gaps)
}


# Assign polygonized sub-pieces to units by largest shared boundary perimeter
sr_assign_sub_pieces <- function(sub_pieces, repaired, candidate_units) {
  adj_bounds <- geos::geos_boundary(repaired[candidate_units])
  for (si in seq_along(sub_pieces)) {
    sp_bound <- geos::geos_boundary(sub_pieces[si])
    perims   <- vapply(seq_along(candidate_units), function(idx) {
      geos::geos_length(geos::geos_intersection(sp_bound, adj_bounds[idx]))
    }, double(1L))
    k <- candidate_units[which.max(perims)]
    repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], sub_pieces[si]))
  }
  repaired
}


#' Fill a triangular gap (3 boundary segments)
#'
#' If all 3 sides touch distinct non-exterior units, splits at the incenter.
#' Otherwise assigns the entire gap to the unit with the largest shared
#' boundary perimeter.
#'
#' @noRd
sr_fill_triangle_gap <- function(gap_geom, repaired, boundaries,
                                  sh_perims, adj_units) {
  non_ext <- unique(Filter(Negate(is.na),
                           vapply(boundaries, `[[`, integer(1L), "unit")))
  if (length(non_ext) == 3L) {
    gap_c <- sr_coords(gap_geom)
    if (nrow(gap_c) >= 3L) {
      ic         <- sr_incenter(gap_c[1:3, c("x", "y")])
      filled_any <- FALSE
      for (b in boundaries) {
        if (is.na(b$unit)) next
        ac   <- wk::wk_coords(b$arc)
        n_ac <- nrow(ac)
        if (n_ac < 2L) next
        tri <- tryCatch(
          geos::as_geos_geometry(sprintf(
            "POLYGON ((%.15g %.15g, %.15g %.15g, %.15g %.15g, %.15g %.15g))",
            ac$x[1L], ac$y[1L], ac$x[n_ac], ac$y[n_ac],
            ic$x, ic$y, ac$x[1L], ac$y[1L]
          )),
          error = function(e) NULL
        )
        if (is.null(tri) || geos::geos_area(tri) <= 0) next
        k <- b$unit
        repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], tri))
        filled_any  <- TRUE
      }
      if (filled_any) return(repaired)
    }
  }
  k <- adj_units[which.max(sh_perims)]
  repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], gap_geom))
  repaired
}


#' Fill a gap with exactly 3 sub-boundaries
#'
#' Handles the 3-sub-boundary (non-triangle) case: incenter-based partition
#' when the incenter of the convex hull of main vertices lies inside the gap,
#' with a shortest-path diagonal fallback.
#'
#' @noRd
sr_fill_3boundary_gap <- function(gap_geom, repaired, bounds_ord,
                                   sh_perims, adj_units) {
  is_ext <- vapply(bounds_ord, function(b) is.na(b$unit), logical(1L))
  n_ext  <- sum(is_ext)

  .fallback <- function() {
    k <- adj_units[which.max(sh_perims)]
    repaired[k] <<- sr_normalize_polygon_geom(geos::geos_union(repaired[k], gap_geom))
    repaired
  }

  .split_via_sp <- function(arc1, arc2, opp_arc, unit1, unit2,
                              main_v_xy, near_xy) {
    sp_df <- tryCatch(
      sr_shortest_path_in_polygon(gap_geom, main_v_xy, near_xy),
      error = function(e) NULL
    )
    if (is.null(sp_df)) return(NULL)
    sp_line <- sr_path_to_linestring(sp_df)
    opp_c   <- wk::wk_coords(opp_arc)
    near_i  <- which(abs(opp_c$x - near_xy$x) < 1e-8 &
                     abs(opp_c$y - near_xy$y) < 1e-8)
    if (length(near_i) == 0L) return(NULL)
    near_i <- near_i[1L]; n_opp <- nrow(opp_c)

    for (lb in list(
      list(b_arc = arc1, k = unit1, pts = opp_c[near_i:n_opp, , drop = FALSE]),
      list(b_arc = arc2, k = unit2, pts = opp_c[1L:near_i,   , drop = FALSE])
    )) {
      if (is.na(lb$k)) next
      opp_part <- sr_path_to_linestring(
        data.frame(x = lb$pts$x, y = lb$pts$y)
      )
      if (is.null(opp_part)) next
      coll  <- geos::geos_make_collection(c(lb$b_arc, sp_line, opp_part))
      noded <- tryCatch(
        geos::geos_node(geos::geos_unary_union(coll)),
        error = function(e) NULL
      )
      if (is.null(noded)) next
      polys <- geos::geos_polygonize(noded)
      if (geos::geos_num_geometries(polys) > 0L) {
        poly <- geos::geos_geometry_n(polys, 1L)
        if (geos::geos_area(poly) > 0)
          repaired[lb$k] <<- sr_normalize_polygon_geom(
            geos::geos_union(repaired[lb$k], poly)
          )
      }
    }
    repaired
  }

  if (n_ext == 0L) {
    # All 3 sub-boundaries are non-exterior
    main_vs <- lapply(bounds_ord, function(b) {
      ac <- wk::wk_coords(b$arc)
      data.frame(x = ac$x[1L], y = ac$y[1L])
    })
    hull_v <- do.call(rbind, main_vs)
    ic_xy  <- sr_incenter(hull_v)
    ic_pt  <- sr_xy_to_point(ic_xy)

    if (isTRUE(geos::geos_contains(gap_geom, ic_pt))) {
      sp_lines <- lapply(main_vs, function(mv) {
        sp_df <- tryCatch(sr_shortest_path_in_polygon(gap_geom, ic_xy, mv),
                          error = function(e) NULL)
        sr_path_to_linestring(sp_df)
      })
      valid_sps <- !vapply(sp_lines, is.null, logical(1L))
      if (sum(valid_sps) >= 2L) {
        all_l_vec <- do.call(c, c(
          list(geos::geos_boundary(gap_geom)),
          Filter(Negate(is.null), sp_lines)
        ))
        noded <- tryCatch(
          geos::geos_node(geos::geos_unary_union(
            geos::geos_make_collection(all_l_vec)
          )),
          error = function(e) NULL
        )
        if (!is.null(noded)) {
          pc  <- geos::geos_polygonize(noded)
          n_s <- geos::geos_num_geometries(pc)
          if (n_s >= 2L) {
            subs <- geos::geos_geometry_n(pc, seq_len(n_s))
            subs <- subs[geos::geos_contains(
              gap_geom, geos::geos_point_on_surface(subs)
            )]
            if (length(subs) > 0L)
              return(sr_assign_sub_pieces(subs, repaired, adj_units))
          }
        }
      }
    }

    # Fallback: shortest-path diagonal to nearest interior point on the boundary
    # closest to the incenter
    arc_dists <- vapply(bounds_ord, function(b)
      geos::geos_distance(b$arc, ic_pt)[[1L]], double(1L))
    min_p <- which.min(arc_dists)
    perm  <- c(min_p, (min_p %% 3L) + 1L, ((min_p + 1L) %% 3L) + 1L)
    b_opp <- bounds_ord[[perm[1L]]]
    b_1   <- bounds_ord[[perm[2L]]]
    b_2   <- bounds_ord[[perm[3L]]]

    ac2       <- wk::wk_coords(b_2$arc)
    main_v_xy <- data.frame(x = ac2$x[1L], y = ac2$y[1L])
    opp_c     <- wk::wk_coords(b_opp$arc)
    n_opp     <- nrow(opp_c)
    if (n_opp <= 2L) return(.fallback())

    int_idx <- 2L:(n_opp - 1L)
    int_c   <- opp_c[int_idx, , drop = FALSE]
    near_i  <- int_idx[which.min(
      sqrt((int_c$x - main_v_xy$x)^2 + (int_c$y - main_v_xy$y)^2)
    )]
    near_xy <- data.frame(x = opp_c$x[near_i], y = opp_c$y[near_i])

    result <- .split_via_sp(b_1$arc, b_2$arc, b_opp$arc,
                             b_1$unit, b_2$unit, main_v_xy, near_xy)
    if (!is.null(result)) return(result)
    return(.fallback())

  } else if (n_ext == 1L) {
    # 1 exterior + 2 non-exterior sub-boundaries
    ext_p <- which(is_ext)[1L]
    perm  <- c(ext_p, (ext_p %% 3L) + 1L, ((ext_p + 1L) %% 3L) + 1L)
    b_ext <- bounds_ord[[perm[1L]]]
    b_1   <- bounds_ord[[perm[2L]]]
    b_2   <- bounds_ord[[perm[3L]]]

    ac2       <- wk::wk_coords(b_2$arc)
    main_v_xy <- data.frame(x = ac2$x[1L], y = ac2$y[1L])
    ext_c     <- wk::wk_coords(b_ext$arc)
    n_ext_c   <- nrow(ext_c)
    near_pos  <- which.min(
      sqrt((ext_c$x - main_v_xy$x)^2 + (ext_c$y - main_v_xy$y)^2)
    )

    if (near_pos == 1L) {
      if (!is.na(b_1$unit))
        repaired[b_1$unit] <- sr_normalize_polygon_geom(
          geos::geos_union(repaired[b_1$unit], gap_geom)
        )
    } else if (near_pos == n_ext_c) {
      if (!is.na(b_2$unit))
        repaired[b_2$unit] <- sr_normalize_polygon_geom(
          geos::geos_union(repaired[b_2$unit], gap_geom)
        )
    } else {
      near_xy <- data.frame(x = ext_c$x[near_pos], y = ext_c$y[near_pos])
      result  <- .split_via_sp(b_1$arc, b_2$arc, b_ext$arc,
                                b_1$unit, b_2$unit, main_v_xy, near_xy)
      if (!is.null(result)) return(result)
      return(.fallback())
    }
    return(repaired)

  } else {
    return(.fallback())
  }
}


#' Fill a gap with 4+ sub-boundaries via iterative diagonal splitting
#'
#' Finds the closest pair of strongly-mutually-visible non-adjacent
#' sub-boundaries, splits the gap along shortest-path diagonals, and returns
#' the remaining sub-gaps for further processing.
#'
#' @return A list `list(repaired, new_gaps)`, or `NULL` if no valid pair found.
#'
#' @noRd
sr_fill_4plus_boundary_gap <- function(gap_geom, repaired, bounds_ord,
                                        sh_perims, adj_units) {
  n_b <- length(bounds_ord)
  path_context <- sr_prepare_shortest_path_context(gap_geom)
  candidate_units <- unique(Filter(Negate(is.na), vapply(bounds_ord, function(bound) {
    if (is.na(bound$unit)) {
      return(NA_integer_)
    }
    as.integer(bound$unit)
  }, integer(1L))))

  # Non-adjacent pairs (positive distance), sorted by distance
  pairs <- list()
  for (i in seq_len(n_b - 1L)) {
    for (j in seq.int(i + 1L, n_b)) {
      d <- geos::geos_distance(bounds_ord[[i]]$arc, bounds_ord[[j]]$arc)[[1L]]
      if (d > 0) pairs <- c(pairs, list(list(i = i, j = j, dist = d)))
    }
  }
  if (length(pairs) == 0L) return(NULL)
  pairs <- pairs[order(vapply(pairs, `[[`, double(1L), "dist"))]

  # Helper: compute remaining gap pieces after assigning some polygons
  .remaining_gaps <- function(new_lines, assigned_polys) {
    all_arcs <- lapply(bounds_ord, `[[`, "arc")
    coll <- geos::geos_make_collection(do.call(c, c(all_arcs, new_lines)))
    nd   <- tryCatch(geos::geos_node(geos::geos_unary_union(coll)),
                     error = function(e) NULL)
    if (is.null(nd)) return(list())
    hp   <- geos::geos_polygonize(nd)
    n_hp <- geos::geos_num_geometries(hp)
    if (n_hp == 0L) return(list())
    all_hp <- geos::geos_geometry_n(hp, seq_len(n_hp))
    gaps   <- vector("list", 0L)
    for (hi in seq_len(n_hp)) {
      if (geos::geos_area(all_hp[hi]) <= 0) next
      rep_pt <- geos::geos_point_on_surface(all_hp[hi])
      already <- any(vapply(assigned_polys, function(p)
        isTRUE(geos::geos_contains(p, rep_pt)), logical(1L)))
      if (!already) gaps[[length(gaps) + 1L]] <- all_hp[hi]
    }
    gaps
  }

  for (pr in pairs) {
    bi <- bounds_ord[[pr$i]]; bj <- bounds_ord[[pr$j]]
    if (is.na(bi$unit) && is.na(bj$unit)) next

    if (is.na(bi$unit) || is.na(bj$unit)) {
      # One exterior + one non-exterior
      b_ext <- if (is.na(bi$unit)) bi else bj
      b_int <- if (is.na(bi$unit)) bj else bi
      k_int <- b_int$unit

      int_c <- wk::wk_coords(b_int$arc); n_int <- nrow(int_c)
      ext_c <- wk::wk_coords(b_ext$arc)
      p1 <- data.frame(x = int_c$x[1L],    y = int_c$y[1L])
      p2 <- data.frame(x = int_c$x[n_int], y = int_c$y[n_int])

      mid_c    <- wk::wk_coords(geos::geos_point_on_surface(b_int$arc))
      near_pos <- which.min(sqrt((ext_c$x - mid_c$x[1L])^2 +
                                   (ext_c$y - mid_c$y[1L])^2))
      near_xy  <- data.frame(x = ext_c$x[near_pos], y = ext_c$y[near_pos])

      path1_df <- tryCatch(sr_shortest_path_in_polygon(gap_geom, p1, near_xy, path_context),
                           error = function(e) NULL)
      path2_df <- tryCatch(sr_shortest_path_in_polygon(gap_geom, p2, near_xy, path_context),
                           error = function(e) NULL)
      if (is.null(path1_df) || is.null(path2_df)) next

      path1_l <- sr_path_to_linestring(path1_df)
      path2_l <- sr_path_to_linestring(path2_df)

      coll_add <- geos::geos_make_collection(c(b_int$arc, path1_l, path2_l))
      nd_add   <- tryCatch(geos::geos_node(geos::geos_unary_union(coll_add)),
                           error = function(e) NULL)
      if (is.null(nd_add)) next
      pa     <- geos::geos_polygonize(nd_add)
      n_add  <- geos::geos_num_geometries(pa)
      if (n_add == 0L) next

      assigned <- list()
      for (pi in seq_len(n_add)) {
        poly <- geos::geos_geometry_n(pa, pi)
        if (geos::geos_area(poly) > 0) {
          repaired[k_int] <- sr_normalize_polygon_geom(
            geos::geos_union(repaired[k_int], poly)
          )
          assigned <- c(assigned, list(poly))
        }
      }
      new_gaps <- .remaining_gaps(list(path1_l, path2_l), assigned)
      return(list(repaired = repaired, new_gaps = new_gaps))
    }

    # Both non-exterior: test strong mutual visibility
    i_c <- wk::wk_coords(bi$arc); n_i <- nrow(i_c)
    j_c <- wk::wk_coords(bj$arc); n_j <- nrow(j_c)
    p11 <- data.frame(x = i_c$x[1L],  y = i_c$y[1L])
    p12 <- data.frame(x = i_c$x[n_i], y = i_c$y[n_i])
    p21 <- data.frame(x = j_c$x[1L],  y = j_c$y[1L])
    p22 <- data.frame(x = j_c$x[n_j], y = j_c$y[n_j])

    tp1 <- tryCatch(sr_shortest_path_in_polygon(gap_geom, p11, p22, path_context),
                    error = function(e) NULL)
    tp2 <- tryCatch(sr_shortest_path_in_polygon(gap_geom, p12, p21, path_context),
                    error = function(e) NULL)
    if (is.null(tp1) || is.null(tp2)) next

    # Paths are disjoint iff they share no vertex coordinates
    tp1_strs <- sprintf("%.6g_%.6g", tp1$x, tp1$y)
    tp2_strs <- sprintf("%.6g_%.6g", tp2$x, tp2$y)
    if (length(intersect(tp1_strs, tp2_strs)) > 0L) next

    # Strongly mutually visible: use crossing paths for repeated targets and
    # non-crossing paths for distinct targets, matching the Python logic.
    if (bi$unit == bj$unit) {
      path1_df <- tp1
      path2_df <- tp2
    } else {
      path1_df <- tryCatch(sr_shortest_path_in_polygon(gap_geom, p11, p21, path_context),
                           error = function(e) NULL)
      path2_df <- tryCatch(sr_shortest_path_in_polygon(gap_geom, p12, p22, path_context),
                           error = function(e) NULL)
    }
    if (is.null(path1_df) || is.null(path2_df)) next

    path1_l <- sr_path_to_linestring(path1_df)
    path2_l <- sr_path_to_linestring(path2_df)

    coll_add <- geos::geos_make_collection(c(bi$arc, bj$arc, path1_l, path2_l))
    nd_add   <- tryCatch(geos::geos_node(geos::geos_unary_union(coll_add)),
                         error = function(e) NULL)
    if (is.null(nd_add)) next
    pa_coll <- geos::geos_polygonize(nd_add)
    n_add   <- geos::geos_num_geometries(pa_coll)
    if (n_add == 0L) next

    pa_vec   <- geos::geos_geometry_n(pa_coll, seq_len(n_add))
    assigned <- list()
    candidate_bounds <- geos::geos_boundary(repaired[candidate_units])
    for (pi in seq_along(pa_vec)) {
      poly <- pa_vec[pi]
      if (geos::geos_area(poly) <= 0) next
      pb      <- geos::geos_boundary(poly)
      perims <- vapply(seq_along(candidate_units), function(idx) {
        geos::geos_length(geos::geos_intersection(pb, candidate_bounds[idx]))
      }, double(1L))
      if (max(perims) <= 0) next
      k <- candidate_units[which.max(perims)]
      if (!is.na(k)) {
        repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], poly))
        assigned    <- c(assigned, list(poly))
      }
    }
    new_gaps <- .remaining_gaps(list(path1_l, path2_l), assigned)
    return(list(repaired = repaired, new_gaps = new_gaps))
  }

  NULL  # no valid pair found
}


#' Close gap pieces by splitting them among adjacent units
#'
#' Consolidates order-0 pieces into connected gap regions, then applies the
#' full algorithm from Clelland (2025): convexification of sub-boundaries
#' followed by case analysis (single unit, triangle, 3 sub-boundaries,
#' 4+ sub-boundaries with iterative diagonal splitting).
#'
#' @param pieces        `geos_geometry` vector of polygon pieces.
#' @param overlap_order Integer vector of overlap orders.
#' @param repaired      `geos_geometry` vector of current repaired geometries.
#' @param n_units       Number of original units.
#' @param max_gap_frac  Maximum gap area as fraction of largest adjacent unit.
#' @param piece_region  Integer region id per piece, or `NULL`.
#' @param unit_region   Integer region id per unit, or `NULL`.
#'
#' @return Updated `repaired` geos_geometry vector.
#'
#' @noRd
sr_close_gaps <- function(
  pieces,
  overlap_order,
  repaired,
  n_units,
  max_gap_frac,
  piece_region = NULL,
  unit_region = NULL
) {
  gap_idxs <- which(overlap_order == 0L)
  if (length(gap_idxs) == 0L) return(repaired)

  # Consolidate adjacent gap pieces into connected regions before processing
  region_aware <- !is.null(piece_region) && !is.null(unit_region)
  gap_vec <- pieces[gap_idxs]

  if (region_aware) {
    gap_regions <- piece_region[gap_idxs]
    gaps_queue <- list()

    for (gap_region in unique(gap_regions[!is.na(gap_regions)])) {
      region_gap_idxs <- which(gap_regions == gap_region)
      region_gaps <- gap_vec[region_gap_idxs]
      if (length(region_gaps) == 0L) {
        next
      }

      if (length(region_gaps) > 1L) {
        gap_union <- geos::geos_unary_union(geos::geos_make_collection(region_gaps))
        n_consol <- geos::geos_num_geometries(gap_union)
        region_queue <- lapply(seq_len(n_consol), function(i) {
          list(
            geom = geos::geos_geometry_n(gap_union, i),
            region = gap_region
          )
        })
      } else {
        region_queue <- list(list(geom = region_gaps[1L], region = gap_region))
      }

      gaps_queue <- c(gaps_queue, region_queue)
    }
  } else {
    if (length(gap_vec) > 1L) {
      gap_union  <- geos::geos_unary_union(geos::geos_make_collection(gap_vec))
      n_consol   <- geos::geos_num_geometries(gap_union)
      gaps_queue <- lapply(seq_len(n_consol), function(i) {
        geos::geos_geometry_n(gap_union, i)
      })
    } else {
      gaps_queue <- list(gap_vec[1L])
    }
  }

  convexified <- sr_convexify_gaps(gaps_queue, repaired, unit_region = unit_region)
  repaired <- convexified$repaired
  gaps_queue <- convexified$gaps_queue

  n_skipped_nsc <- 0L
  n_skipped_area <- 0L

  while (length(gaps_queue) > 0L) {
    gap_item   <- gaps_queue[[1L]]
    gaps_queue <- gaps_queue[-1L]
    if (region_aware) {
      gap_geom <- gap_item$geom
      gap_region <- gap_item$region
    } else {
      gap_geom <- gap_item
      gap_region <- NULL
    }

    if (geos::geos_is_empty(gap_geom)) next
    if (geos::geos_num_interior_rings(gap_geom) > 0L) {
      n_skipped_nsc <- n_skipped_nsc + 1L
      next
    }

    adjacency <- sr_gap_adjacency(gap_geom, repaired)
    if (is.null(adjacency)) next
    adj_units <- adjacency$candidates
    sh_perims <- adjacency$shared_perims

    if (region_aware && !is.na(gap_region)) {
      keep <- unit_region[adj_units] == gap_region
      keep[is.na(keep)] <- FALSE
      adj_units <- adj_units[keep]
      sh_perims <- sh_perims[keep]
      if (length(adj_units) == 0L) next
    }

    # Skip gap if too large relative to adjacent units
    gap_area <- geos::geos_area(gap_geom)
    max_a    <- max(geos::geos_area(repaired[adj_units]))
    if (max_a > 0 && gap_area > max_gap_frac * max_a) {
      n_skipped_area <- n_skipped_area + 1L
      next
    }

    # Trivial: single adjacent unit
    if (length(adj_units) == 1L) {
      k <- adj_units[1L]
      repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], gap_geom))
      next
    }

    # Build and order sub-boundaries for case analysis
    bounds <- tryCatch(
      sr_construct_gap_boundaries(gap_geom, repaired, adj_units),
      error = function(e) NULL
    )
    if (is.null(bounds) || length(bounds) == 0L) {
      k <- adj_units[which.max(sh_perims)]
      repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], gap_geom))
      next
    }
    n_non_ext <- sum(vapply(bounds, function(b) !is.na(b$unit), logical(1L)))
    if (n_non_ext == 0L) next
    if (n_non_ext == 1L) {
      k <- Filter(function(b) !is.na(b$unit), bounds)[[1L]]$unit
      repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], gap_geom))
      next
    }

    bounds_ord <- sr_merge_adjacent_boundaries(sr_order_boundaries(gap_geom, bounds))
    n_g_segs   <- nrow(sr_coords(gap_geom))  # vertices == segments in closed ring
    n_bounds   <- length(bounds_ord)

    result <- if (n_g_segs == 3L) {
      # Triangular gap: incenter split or perimeter fallback
      tryCatch(
        sr_fill_triangle_gap(gap_geom, repaired, bounds_ord, sh_perims, adj_units),
        error = function(e) NULL
      )
    } else if (n_bounds == 3L) {
      # 3 sub-boundaries: incenter or diagonal split
      tryCatch(
        sr_fill_3boundary_gap(gap_geom, repaired, bounds_ord, sh_perims, adj_units),
        error = function(e) NULL
      )
    } else if (n_bounds >= 4L) {
      # 4+ sub-boundaries: iterative diagonal splitting
      r4 <- tryCatch(
        sr_fill_4plus_boundary_gap(gap_geom, repaired, bounds_ord,
                                    sh_perims, adj_units),
        error = function(e) NULL
      )
      if (!is.null(r4)) {
        repaired   <- r4$repaired
        if (region_aware) {
          new_gap_items <- lapply(as.list(r4$new_gaps), function(new_gap) {
            list(geom = new_gap, region = gap_region)
          })
          gaps_queue <- c(new_gap_items, gaps_queue)
        } else {
          gaps_queue <- c(as.list(r4$new_gaps), gaps_queue)
        }
        next
      }
      NULL
    } else {
      NULL
    }

    if (!is.null(result)) {
      repaired <- result
    } else {
      k <- adj_units[which.max(sh_perims)]
      repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], gap_geom))
    }
  }

  if (n_skipped_nsc > 0L) {
    cli::cli_inform(
      '{n_skipped_nsc} gap{?s} {?was/were} not filled because {?it is/they are} \\
      not simply connected.'
    )
  }
  if (n_skipped_area > 0L) {
    cli::cli_inform(
      '{n_skipped_area} gap{?s} {?was/were} not filled because {?it/they} \\
      exceed{?s/} the area threshold.'
    )
  }

  repaired
}
#'
#' This removes any positive-area overlaps that may have been introduced by
#' geometric unions during gap-closing or later post-processing, while staying
#' entirely in geos-land.
#'
#' @param repaired `geos_geometry` vector of repaired unit geometries.
#'
#' @return A normalized `geos_geometry` vector with overlaps reassigned away.
#'
#' @noRd
sr_rebuild_repaired_geom <- function(repaired) {
  n_units <- length(repaired)
  refined <- sr_construct_refined_tiling(repaired, NULL, NULL, NULL)
  piece_assignment <- sr_assign_overlaps(
    refined$pieces,
    refined$piece_parents,
    refined$overlap_order,
    n_units
  )
  sr_normalize_polygon_geom(
    sr_build_repaired_geom(refined$pieces, piece_assignment, n_units)
  )
}


# --- Phase 4 helpers: cleanup disconnected units ------------------------

#' Reassign small orphaned components of disconnected units
#'
#' Only units that became *more* disconnected during repair (more components
#' than in the original) are considered.  For each such unit, the smallest
#' excess components are reassigned to the adjacent unit sharing the largest
#' boundary perimeter, provided the component area is below
#' `min_component_frac * max(repaired_area, original_area)`.
#'
#' @param repaired `geos_geometry` vector of repaired unit geometries.
#' @param min_component_frac Threshold fraction for orphan reassignment.
#' @param original_n_comps Integer vector of original component counts.
#' @param original_geom `geos_geometry` vector of original unit geometries.
#'
#' @return Updated `geos_geometry` vector.
#'
#' @noRd
sr_cleanup_disconnected <- function(repaired, min_component_frac,
                                    original_n_comps = NULL,
                                    original_geom = NULL) {
  n_units <- length(repaired)
  changed <- TRUE

  while (changed) {
    changed <- FALSE

    for (k in seq_len(n_units)) {
      if (geos::geos_is_empty(repaired[k])) next
      n_comp <- geos::geos_num_geometries(repaired[k])
      if (n_comp <= 1L) next

      # Only process units with more components than originally
      orig_n <- if (!is.null(original_n_comps)) original_n_comps[k] else 1L
      excess <- n_comp - orig_n
      if (excess <= 0L) next

      # Threshold: fraction of max(repaired_area, original_area)
      repaired_area <- geos::geos_area(repaired[k])
      orig_area <- if (!is.null(original_geom)) geos::geos_area(original_geom[k]) else repaired_area
      big_area <- max(repaired_area, orig_area)
      orphan_limit <- min_component_frac * big_area

      # Extract components sorted by area
      comps <- geos::geos_geometry_n(repaired[k], seq_len(n_comp))
      areas <- geos::geos_area(comps)
      ord <- order(areas)

      # Reassign the `excess` smallest components (if below threshold)
      remove_idxs <- integer(0L)
      for (ii in seq_len(min(excess, n_comp - 1L))) {
        comp_idx <- ord[ii]
        if (areas[comp_idx] >= orphan_limit) next

        smallest_comp <- comps[comp_idx]
        repaired_tree <- geos::geos_strtree(repaired)
        candidates <- geos::geos_strtree_query(repaired_tree, smallest_comp)[[1]]
        candidates <- setdiff(candidates, k)
        if (length(candidates) == 0L) next

        comp_bound <- geos::geos_boundary(smallest_comp)
        shared_perims <- vapply(
          candidates,
          function(j) {
            if (geos::geos_is_empty(repaired[j])) return(0)
            geos::geos_length(
              geos::geos_intersection(comp_bound, geos::geos_boundary(repaired[j]))
            )
          },
          double(1)
        )

        best_perim <- max(shared_perims)
        if (best_perim <= 0) next
        best_k <- candidates[which.max(shared_perims)]

        repaired[best_k] <- sr_normalize_polygon_geom(
          geos::geos_union(repaired[best_k], smallest_comp)
        )
        remove_idxs <- c(remove_idxs, comp_idx)
        changed <- TRUE
      }

      # Rebuild unit k without the reassigned components
      if (length(remove_idxs) > 0L) {
        remaining <- comps[setdiff(seq_len(n_comp), remove_idxs)]
        if (length(remaining) == 0L) {
          repaired[k] <- geos::as_geos_geometry("POLYGON EMPTY")
        } else if (length(remaining) == 1L) {
          repaired[k] <- remaining[1L]
        } else {
          repaired[k] <- sr_normalize_polygon_geom(
            geos::geos_unary_union(geos::geos_make_collection(remaining))
          )
        }
      }
    }
  }

  # Inform about remaining disconnected units (that got worse)
  still_disc <- which(
    !geos::geos_is_empty(repaired) &
      geos::geos_num_geometries(repaired) > 1L
  )
  if (!is.null(original_n_comps)) {
    still_disc <- still_disc[
      geos::geos_num_geometries(repaired[still_disc]) > original_n_comps[still_disc]
    ]
  }
  if (length(still_disc) > 0L) {
    cli::cli_inform(
      '{length(still_disc)} unit{?s} remain{?s/} disconnected after repair.'
    )
  }

  repaired
}


# --- Phase 5 helpers: rook-to-queen conversion --------------------------

#' Convert short rook adjacencies to queen adjacencies
#'
#' For each pair of repaired units whose *total* shared boundary length is
#' shorter than `rook_threshold`, excise a small disk around each boundary
#' component's midpoint and redistribute the resulting gap to adjacent units.
#' The threshold is applied to the aggregate length per pair (not per segment),
#' matching the Python implementation.
#'
#' @param repaired `geos_geometry` vector of repaired unit geometries.
#' @param rook_threshold Numeric threshold (in planar CRS units). `NULL` or
#'   non-positive skips this phase.
#'
#' @return Updated `geos_geometry` vector.
#'
#' @noRd
sr_rook_to_queen <- function(repaired, rook_threshold) {
  if (is.null(rook_threshold) || rook_threshold <= 0) {
    return(repaired)
  }

  n_units <- length(repaired)
  non_empty <- which(!geos::geos_is_empty(repaired))
  if (length(non_empty) < 2L) {
    return(repaired)
  }

  repeat {
    tree <- geos::geos_strtree(repaired)

    # Collect per-pair total shared lengths, then build disks only for short pairs
    pair_info <- list()
    for (i in non_empty) {
      candidates <- geos::geos_strtree_query(tree, repaired[i])[[1]]
      candidates <- candidates[candidates > i]

      for (j in candidates) {
        if (geos::geos_is_empty(repaired[j])) next
        if (!geos::geos_intersects(repaired[i], repaired[j])) next

        shared <- geos::geos_intersection(
          geos::geos_boundary(repaired[i]),
          geos::geos_boundary(repaired[j])
        )
        total_len <- geos::geos_length(shared)

        if (total_len > 0 && total_len < rook_threshold) {
          # Explode into individual line components for disk placement
          shared_type <- geos::geos_type(shared)
          if (grepl("MULTI|COLLECTION", shared_type, ignore.case = TRUE)) {
            n_parts <- geos::geos_num_geometries(shared)
            parts <- geos::geos_geometry_n(shared, seq_len(n_parts))
            parts <- parts[geos::geos_length(parts) > 0]
          } else if (geos::geos_length(shared) > 0) {
            parts <- shared
          } else {
            next
          }

          for (p_idx in seq_along(parts)) {
            seg <- parts[p_idx]
            seg_len <- geos::geos_length(seg)
            if (seg_len <= 0) next
            midpt <- geos::geos_centroid(seg)
            radius <- 0.6 * seg_len
            pair_info[[length(pair_info) + 1L]] <- list(
              disk = geos::geos_buffer(midpt, radius)
            )
          }
        }
      }
    }

    if (length(pair_info) == 0L) break

    # Merge overlapping disks, then iteratively convexify until disjoint
    all_disks <- do.call(c, lapply(pair_info, `[[`, "disk"))
    polys_to_remove <- all_disks
    repeat {
      merged <- geos::geos_unary_union(geos::geos_make_collection(polys_to_remove))
      n_merged <- geos::geos_num_geometries(merged)
      if (n_merged == 0L) break
      merged_parts <- geos::geos_geometry_n(merged, seq_len(n_merged))
      convex_parts <- geos::geos_convex_hull(merged_parts)

      if (n_merged == 1L) {
        polys_to_remove <- convex_parts
        break
      }

      convex_union <- geos::geos_unary_union(geos::geos_make_collection(convex_parts))
      n_convex <- geos::geos_num_geometries(convex_union)
      if (n_convex == n_merged) {
        polys_to_remove <- convex_parts
        break
      }
      polys_to_remove <- convex_parts
    }
    merged_disks <- polys_to_remove

    # Process each merged disk
    for (d_idx in seq_along(merged_disks)) {
      disk <- merged_disks[d_idx]

      # Find affected units
      affected <- geos::geos_strtree_query(tree, disk)[[1]]
      affected <- affected[!geos::geos_is_empty(repaired[affected])]
      affected <- affected[geos::geos_intersects(repaired[affected], disk)]
      if (length(affected) == 0L) next

      # Excise disk from affected units
      for (k in affected) {
        repaired[k] <- geos::geos_difference(repaired[k], disk)
      }

      # Fill disk gap: nod disk boundary + affected unit boundaries, polygonize
      all_bounds <- c(
        geos::geos_boundary(disk),
        geos::geos_boundary(repaired[affected])
      )
      coll <- geos::geos_make_collection(all_bounds)
      noded <- geos::geos_node(geos::geos_unary_union(coll))
      poly_coll <- geos::geos_polygonize(noded)
      n_sub <- geos::geos_num_geometries(poly_coll)
      if (n_sub == 0L) next

      sub_pieces <- geos::geos_geometry_n(poly_coll, seq_len(n_sub))

      # Keep only pieces inside the disk
      sub_reps <- geos::geos_point_on_surface(sub_pieces)
      inside_disk <- geos::geos_contains(disk, sub_reps)
      sub_pieces <- sub_pieces[inside_disk]
      if (length(sub_pieces) == 0L) next

      # Assign each piece to adjacent unit with largest shared perimeter
      for (si in seq_along(sub_pieces)) {
        sp_bound <- geos::geos_boundary(sub_pieces[si])
        perims <- vapply(
          affected,
          function(k) {
            if (geos::geos_is_empty(repaired[k])) return(0)
            geos::geos_length(
              geos::geos_intersection(sp_bound, geos::geos_boundary(repaired[k]))
            )
          },
          double(1)
        )
        best <- affected[which.max(perims)]
        repaired[best] <- sr_normalize_polygon_geom(
          geos::geos_union(repaired[best], sub_pieces[si])
        )
      }
    }
  }

  repaired
}
