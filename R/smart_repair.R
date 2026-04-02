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
  }

  if (max_gap_frac < 0 || max_gap_frac > 1) {
    cli::cli_abort('{.arg max_gap_frac} must be between 0 and 1.')
  }
  if (min_component_frac < 0 || min_component_frac > 1) {
    cli::cli_abort('{.arg min_component_frac} must be between 0 and 1.')
  }

  n_units <- nrow(shp)
  original_crs <- sf::st_crs(shp)

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
    sf::st_geometry(shp) <- sf::st_collection_extract(fixed, type = 'POLYGON')
  }
  if (region_aware) {
    invalid_regions <- which(!sf::st_is_valid(regions))
    if (length(invalid_regions) > 0L) {
      cli::cli_warn(
        '{length(invalid_regions)} region{?s} {?has/have} invalid geometr{?y/ies}; \\
        repairing with {.fn sf::st_make_valid} before processing.'
      )
      fixed_r <- sf::st_make_valid(sf::st_geometry(regions))
      sf::st_geometry(regions) <- sf::st_collection_extract(fixed_r, type = 'POLYGON')
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

  # PHASE 0 (optional): Assign units to regions
  unit_region <- sr_assign_units_to_regions(unit_geom, region_geom)

  # PHASE 1: Construct refined tiling
  refined <- sr_construct_refined_tiling(unit_geom, region_geom, unit_region)

  pieces <- refined$pieces
  piece_parents <- refined$piece_parents
  overlap_order <- refined$overlap_order

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
    max_gap_frac
  )

  # PHASE 4: Clean up disconnected units
  repaired <- sr_normalize_polygon_geom(repaired)
  repaired <- sr_cleanup_disconnected(repaired, min_component_frac)

  # PHASE 5 (optional): Small rook-to-queen adjacency conversion
  repaired <- sr_normalize_polygon_geom(repaired)
  repaired <- sr_rook_to_queen(repaired, rook_threshold)

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
sr_construct_refined_tiling <- function(unit_geom, region_geom, unit_region) {
  # -- 1a: fully noded union of boundaries --------------------------------
  noded <- sr_compute_noded_union(unit_geom, region_geom)

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
sr_compute_noded_union <- function(unit_geom, region_geom) {
  unit_bounds <- geos::geos_boundary(unit_geom)

  if (!is.null(region_geom)) {
    region_bounds <- geos::geos_boundary(region_geom)
    all_bounds <- c(unit_bounds, region_bounds)
  } else {
    all_bounds <- unit_bounds
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
#' @return A list with `pieces`, `piece_parents`, and `overlap_order` (see
#'   `sr_construct_refined_tiling()` for details).
#'
#' @noRd
sr_tag_pieces <- function(pieces, unit_geom, region_geom, unit_region) {
  n_pieces <- length(pieces)
  region_aware <- !is.null(region_geom) && !is.null(unit_region)

  # Representative point inside each piece (guaranteed to be interior)
  rep_pts <- geos::geos_point_on_surface(pieces)

  unit_tree <- geos::geos_strtree(unit_geom)

  # For each rep point, find which units' bounding boxes it may fall in
  candidates <- geos::geos_strtree_query(unit_tree, rep_pts)

  # If region-aware, also determine which region each piece belongs to
  piece_region <- NULL
  if (region_aware) {
    piece_region <- largest_intersection_geos(rep_pts, region_geom)
  }

  # For each piece, refine candidates to true containment
  piece_parents <- vector('list', n_pieces)
  for (i in seq_len(n_pieces)) {
    cands <- candidates[[i]]
    if (length(cands) == 0L) {
      piece_parents[[i]] <- integer(0L)
      next
    }

    # Test actual containment: does the unit polygon contain the rep point?
    inside <- geos::geos_contains(unit_geom[cands], rep_pts[[i]])
    parents <- cands[inside]

    # In region-aware mode, keep only parents assigned to the same region
    if (region_aware && !is.na(piece_region[i])) {
      parents <- parents[unit_region[parents] == piece_region[i]]
    }

    piece_parents[[i]] <- as.integer(parents)
  }

  overlap_order <- lengths(piece_parents)

  # In region-aware mode, drop pieces outside all regions
  if (region_aware) {
    keep <- !is.na(piece_region)
    pieces <- pieces[keep]
    piece_parents <- piece_parents[keep]
    overlap_order <- overlap_order[keep]
  }

  list(
    pieces = pieces,
    piece_parents = piece_parents,
    overlap_order = as.integer(overlap_order)
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

#' Close gap pieces by splitting them among adjacent units
#'
#' For each gap (order-0 piece), find adjacent repaired units and split the gap
#' proportionally using a centroid-based triangular decomposition. Each triangle
#' is assigned to the adjacent unit whose boundary it shares the most perimeter
#' with. This preserves adjacency relations (the key innovation over naive
#' "assign entire gap to one unit" approaches).
#'
#' Gaps that are non-simply-connected or exceed `max_gap_frac` times the
#' largest adjacent unit area are skipped.
#'
#' @param pieces `geos_geometry` vector of polygon pieces.
#' @param overlap_order Integer vector of overlap orders.
#' @param repaired `geos_geometry` vector of current repaired geometries.
#' @param n_units Number of units.
#' @param max_gap_frac Maximum gap area as fraction of largest adjacent unit.
#'
#' @return Updated `repaired` geos_geometry vector.
#'
#' @noRd
sr_close_gaps <- function(
  pieces,
  overlap_order,
  repaired,
  n_units,
  max_gap_frac
) {
  gaps <- which(overlap_order == 0L)
  if (length(gaps) == 0L) {
    return(repaired)
  }

  # Spatial index on repaired units for efficient lookup
  repaired_tree <- geos::geos_strtree(repaired)

  for (i in gaps) {
    gap_geom <- pieces[i]

    # Skip non-simply-connected gaps (have holes)
    if (geos::geos_num_interior_rings(gap_geom) > 0L) {
      next
    }

    # Find candidate adjacent units via spatial index
    candidates <- geos::geos_strtree_query(repaired_tree, gap_geom)[[1]]
    if (length(candidates) == 0L) {
      next
    }

    # Filter to units actually sharing a boundary (positive shared length)
    gap_bound <- geos::geos_boundary(gap_geom)
    shared_perims <- vapply(
      candidates,
      function(k) {
        if (geos::geos_is_empty(repaired[k])) {
          return(0)
        }
        inter <- geos::geos_intersection(
          gap_bound,
          geos::geos_boundary(repaired[k])
        )
        geos::geos_length(inter)
      },
      double(1)
    )

    adjacent_mask <- shared_perims > 0
    if (!any(adjacent_mask)) {
      next
    }

    adjacent_units <- candidates[adjacent_mask]
    shared_perims <- shared_perims[adjacent_mask]

    # Skip gaps that are too large relative to adjacent units
    gap_area <- geos::geos_area(gap_geom)
    max_adj_area <- max(geos::geos_area(repaired[adjacent_units]))
    if (max_adj_area > 0 && gap_area > max_gap_frac * max_adj_area) {
      next
    }

    if (length(adjacent_units) == 1L) {
      # Single adjacent unit: assign whole gap
      k <- adjacent_units[1L]
      repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], gap_geom))
    } else {
      # Multiple adjacent units: split gap via centroid decomposition
      split <- sr_split_gap(gap_geom, repaired, adjacent_units)

      if (is.null(split)) {
        # Splitting failed — fall back to largest shared perimeter
        k <- adjacent_units[which.max(shared_perims)]
        repaired[k] <- sr_normalize_polygon_geom(geos::geos_union(repaired[k], gap_geom))
      } else {
        for (si in seq_along(split$sub_pieces)) {
          k <- split$assignments[si]
          repaired[k] <- sr_normalize_polygon_geom(
            geos::geos_union(repaired[k], split$sub_pieces[si])
          )
        }
      }
    }
  }

  repaired
}


#' Split a gap polygon among adjacent units via centroid decomposition
#'
#' Creates a triangular fan from the gap's interior point to each boundary
#' vertex, then assigns each triangle to the adjacent unit sharing the most
#' boundary with it.
#'
#' @param gap_geom A single `geos_geometry` polygon (the gap).
#' @param repaired `geos_geometry` vector of current repaired unit geometries.
#' @param adjacent_units Integer vector of unit indices adjacent to this gap.
#'
#' @return A list with `sub_pieces` (geos_geometry vector) and `assignments`
#'   (integer vector of unit indices), or `NULL` if splitting fails.
#'
#' @noRd
sr_split_gap <- function(gap_geom, repaired, adjacent_units) {
  # Use point_on_surface for robustness (always inside the polygon)
  center <- geos::geos_point_on_surface(gap_geom)

  # Extract gap boundary vertices via wk
  coords <- wk::wk_coords(gap_geom)
  n_v <- nrow(coords)
  # Remove closing vertex (duplicate of first)
  if (
    n_v > 1 &&
      coords$x[1] == coords$x[n_v] &&
      coords$y[1] == coords$y[n_v]
  ) {
    coords <- coords[-n_v, , drop = FALSE]
    n_v <- n_v - 1L
  }

  if (n_v < 3L) {
    return(NULL)
  }

  # Create line segments from center to each vertex
  center_coords <- wk::wk_coords(center)
  cx <- center_coords$x[1]
  cy <- center_coords$y[1]

  wkt_lines <- sprintf(
    "LINESTRING (%.15g %.15g, %.15g %.15g)",
    cx,
    cy,
    coords$x,
    coords$y
  )
  split_lines <- geos::as_geos_geometry(wkt_lines)

  # Nod gap boundary + split lines, then polygonize.
  # geos_node can throw TopologyException for near-degenerate geometries;
  # return NULL to trigger the fallback (assign whole gap by largest perimeter).
  all_lines <- c(geos::geos_boundary(gap_geom), split_lines)
  coll <- geos::geos_make_collection(all_lines)
  unioned <- geos::geos_unary_union(coll)
  noded <- tryCatch(geos::geos_node(unioned), error = function(e) NULL)
  if (is.null(noded)) {
    return(NULL)
  }
  poly_coll <- geos::geos_polygonize(noded)
  n_sub <- geos::geos_num_geometries(poly_coll)

  if (n_sub <= 1L) {
    return(NULL)
  }

  sub_pieces <- geos::geos_geometry_n(poly_coll, seq_len(n_sub))

  # Only keep pieces whose representative point is inside the gap
  sub_reps <- geos::geos_point_on_surface(sub_pieces)
  inside <- geos::geos_contains(gap_geom, sub_reps)
  sub_pieces <- sub_pieces[inside]
  if (length(sub_pieces) == 0L) {
    return(NULL)
  }

  # Assign each sub-piece to adjacent unit with largest shared perimeter
  adj_bounds <- geos::geos_boundary(repaired[adjacent_units])
  assignments <- integer(length(sub_pieces))
  for (si in seq_along(sub_pieces)) {
    sp_bound <- geos::geos_boundary(sub_pieces[si])
    perims <- vapply(
      seq_along(adjacent_units),
      function(idx) {
        geos::geos_length(
          geos::geos_intersection(sp_bound, adj_bounds[idx])
        )
      },
      double(1)
    )
    assignments[si] <- adjacent_units[which.max(perims)]
  }

  list(sub_pieces = sub_pieces, assignments = assignments)
}


# --- Phase 4 helpers: cleanup disconnected units ------------------------

#' Reassign small orphaned components of disconnected units
#'
#' For each unit that is a multipolygon (disconnected), iteratively reassign
#' the smallest component to the adjacent unit sharing the largest boundary
#' perimeter, provided the component is smaller than `min_component_frac`
#' times the unit's largest component.
#'
#' @param repaired `geos_geometry` vector of repaired unit geometries.
#' @param min_component_frac Threshold fraction for orphan reassignment.
#'
#' @return Updated `geos_geometry` vector.
#'
#' @noRd
sr_cleanup_disconnected <- function(repaired, min_component_frac) {
  n_units <- length(repaired)
  repaired_tree <- geos::geos_strtree(repaired)

  for (k in seq_len(n_units)) {
    repeat {
      if (geos::geos_is_empty(repaired[k])) {
        break
      }
      n_comp <- geos::geos_num_geometries(repaired[k])
      if (n_comp <= 1L) {
        break
      }

      # Extract components and sort by area
      comps <- geos::geos_geometry_n(repaired[k], seq_len(n_comp))
      areas <- geos::geos_area(comps)
      ord <- order(areas)
      smallest_idx <- ord[1L]
      largest_area <- areas[ord[n_comp]]
      smallest_area <- areas[smallest_idx]

      if (smallest_area >= min_component_frac * largest_area) {
        break
      }

      # Find adjacent unit for the orphan component
      smallest_comp <- comps[smallest_idx]
      candidates <- geos::geos_strtree_query(repaired_tree, smallest_comp)[[1]]
      candidates <- setdiff(candidates, k)

      if (length(candidates) == 0L) {
        break
      }

      comp_bound <- geos::geos_boundary(smallest_comp)
      shared_perims <- vapply(
        candidates,
        function(j) {
          if (geos::geos_is_empty(repaired[j])) {
            return(0)
          }
          inter <- geos::geos_intersection(
            comp_bound,
            geos::geos_boundary(repaired[j])
          )
          geos::geos_length(inter)
        },
        double(1)
      )

      best_perim <- max(shared_perims)
      if (best_perim <= 0) {
        break
      }
      best_k <- candidates[which.max(shared_perims)]

      # Reassign: remove orphan from k, add to best_k
      remaining <- comps[setdiff(seq_len(n_comp), smallest_idx)]
      if (length(remaining) == 1L) {
        repaired[k] <- remaining[1L]
      } else {
        repaired[k] <- sr_normalize_polygon_geom(
          geos::geos_unary_union(geos::geos_make_collection(remaining))
        )
      }
      repaired[best_k] <- sr_normalize_polygon_geom(
        geos::geos_union(repaired[best_k], smallest_comp)
      )
    }
  }

  # Inform about remaining disconnected units
  still_disc <- which(
    !geos::geos_is_empty(repaired) &
      geos::geos_num_geometries(repaired) > 1L
  )
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
#' For each pair of repaired units sharing a boundary shorter than
#' `rook_threshold`, excise a small disk around the shared boundary midpoint
#' and redistribute the resulting gap to adjacent units using the noded-union +
#' polygonize approach. This effectively replaces the rook adjacency (shared
#' edge) with a queen adjacency (shared point).
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

  tree <- geos::geos_strtree(repaired)

  # Collect disks for all short rook adjacencies
  disk_list <- list()
  for (i in non_empty) {
    candidates <- geos::geos_strtree_query(tree, repaired[i])[[1]]
    candidates <- candidates[candidates > i]

    for (j in candidates) {
      if (geos::geos_is_empty(repaired[j])) {
        next
      }
      if (!geos::geos_intersects(repaired[i], repaired[j])) {
        next
      }

      shared <- geos::geos_intersection(
        geos::geos_boundary(repaired[i]),
        geos::geos_boundary(repaired[j])
      )
      shared_len <- geos::geos_length(shared)

      if (shared_len > 0 && shared_len < rook_threshold) {
        midpt <- geos::geos_centroid(shared)
        radius <- shared_len / 2 * 1.1
        disk_list[[length(disk_list) + 1L]] <- geos::geos_buffer(midpt, radius)
      }
    }
  }

  if (length(disk_list) == 0L) {
    return(repaired)
  }

  # Merge overlapping disks
  all_disks <- do.call(c, disk_list)
  merged <- geos::geos_unary_union(geos::geos_make_collection(all_disks))
  n_merged <- geos::geos_num_geometries(merged)
  if (n_merged == 0L) {
    return(repaired)
  }
  merged_disks <- geos::geos_geometry_n(merged, seq_len(n_merged))

  # Process each merged disk
  for (d_idx in seq_along(merged_disks)) {
    disk <- merged_disks[d_idx]

    # Find affected units
    affected <- geos::geos_strtree_query(tree, disk)[[1]]
    affected <- affected[!geos::geos_is_empty(repaired[affected])]
    affected <- affected[geos::geos_intersects(repaired[affected], disk)]
    if (length(affected) == 0L) {
      next
    }

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
    if (n_sub == 0L) {
      next
    }

    sub_pieces <- geos::geos_geometry_n(poly_coll, seq_len(n_sub))

    # Keep only pieces inside the disk
    sub_reps <- geos::geos_point_on_surface(sub_pieces)
    inside_disk <- geos::geos_contains(disk, sub_reps)
    sub_pieces <- sub_pieces[inside_disk]
    if (length(sub_pieces) == 0L) {
      next
    }

    # Assign each piece to adjacent unit with largest shared perimeter
    for (si in seq_along(sub_pieces)) {
      sp_bound <- geos::geos_boundary(sub_pieces[si])
      perims <- vapply(
        affected,
        function(k) {
          if (geos::geos_is_empty(repaired[k])) {
            return(0)
          }
          geos::geos_length(
            geos::geos_intersection(sp_bound, geos::geos_boundary(repaired[k]))
          )
        },
        double(1)
      )
      best <- affected[which.max(perims)]
      repaired[best] <- geos::geos_union(repaired[best], sub_pieces[si])
    }
  }

  repaired
}
