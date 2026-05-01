# Changelog

## geomander 2.5.3

- Fixes an NA matching error within
  [`get_lewis()`](https://christophertkenny.com/geomander/reference/get_lewis.md)
  that would break downloads for certain congress-state pairs
  ([\#17](https://github.com/christopherkenny/geomander/issues/17)).
- Expands inline documentation to provide better descriptions of
  arguments and goals of functions.

## geomander 2.5.2

CRAN release: 2025-12-11

- Fixes a CRAN download issue for invalid links for Lewis, DeVine,
  Pitcher, and Martis shapefiles.

## geomander 2.5.1

- Updates Maine for
  [`get_alarm()`](https://christophertkenny.com/geomander/reference/get_alarm.md)
  to grab updated files that have full coverage for the state.
- Fixes an issue where
  [`block2prec()`](https://christophertkenny.com/geomander/reference/block2prec.md)
  would fail if there were units in `to` that had no matches in `from`.

## geomander 2.5.0

CRAN release: 2025-09-01

- Adds large speed improvements to
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md).

## geomander 2.4.0

- Replaces magrittr pipe with the base R pipe internally and in
  examples.
- Improves handling of columns within
  [`block2prec()`](https://christophertkenny.com/geomander/reference/block2prec.md).
- Adds an attribute to
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md)
  when the last precinct in `to` is not matched. This can later be used
  when filling missing precincts (the implicit 0s).

## geomander 2.3.0

CRAN release: 2024-02-15

- Adds support for downloading Jeffrey B. Lewis’s historical
  congressional districts with \`get_lewis()
- Adds support for downloading RPV Near Me datasets with
  [`get_rpvnearme()`](https://christophertkenny.com/geomander/reference/get_rpvnearme.md)
- Corrects normalized global Moran’s I calculation
  ([\#12](https://github.com/christopherkenny/geomander/issues/12)),
  thanks [@CoryMcCartan](https://github.com/CoryMcCartan).
- Fixes a download issue for
  [`get_dra()`](https://christophertkenny.com/geomander/reference/get_dra.md)

## geomander 2.2.0

- Updates dependencies for easier installation.
- Allows for use of `circle` as a method in
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md).
- Adds a `by` argument to
  [`geo_match()`](https://christophertkenny.com/geomander/reference/geo_match.md)
  for subsetting to known matching regions.
- Uses `redist` in vignettes only conditionally.
- Massive improvements in performance for
  [`check_contiguity()`](https://christophertkenny.com/geomander/reference/check_contiguity.md).

## geomander 2.1.1

- Fixes missing tolerance bug in
  [`geos_circle_center()`](https://christophertkenny.com/geomander/reference/geos_circle_center.md).
- Adds experimental regionalizion approach,
  [`regionalize()`](https://christophertkenny.com/geomander/reference/regionalize.md).

## geomander 2.1.0

CRAN release: 2022-06-23

- Avoid indirect download for
  [`get_alarm()`](https://christophertkenny.com/geomander/reference/get_alarm.md).
  Fixes bug where block level data resulted in mismatched geometry.
- Add
  [`st_circle_center()`](https://christophertkenny.com/geomander/reference/st_circle_center.md)
  and
  [`geos_circle_center()`](https://christophertkenny.com/geomander/reference/geos_circle_center.md)
  for centroid of the maximum inscribed circle.
- Add
  [`baf_to_vtd()`](https://christophertkenny.com/geomander/reference/baf_to_vtd.md)
  function to approximate BAFs at the VTD level.

## geomander 2.0.2

CRAN release: 2021-12-08

- Drop `spdep` dependency to avoid timeouts on checking examples.

## geomander 2.0.0

- [`check_contiguity()`](https://christophertkenny.com/geomander/reference/check_contiguity.md)
  orders by most frequent component, so less common components have
  higher numbers.
- Uses `geos` for geographic computations over `sf` to avoid `s2` where
  possible and introduce speed improvements.
- Uses `cli` package for progress reporting.
- `adjacency` renamed to `adj` to match `redist` naming.
- Updates `r2dra` and `dra2r` to work off of 2020 geometries.
- Majority of functions gain `epsg` argument to project for accuracy
  relative to projection.

## geomander 1.1.2

- Fixes website redirects for CRAN.
- Improves flexibility in `block2prec`.
- Updates default year to 2020 for
  [`create_block_table()`](https://christophertkenny.com/geomander/reference/create_block_table.md).
- Adds a polygon contiguity check. Slower than the standard contiguity
  check, but covers more edge cases.

## geomander 1.1.1

- Replace `PL94171` import with `tigris`.

## geomander 1.1.0

- Collapse county calls into one for
  [`create_block_table()`](https://christophertkenny.com/geomander/reference/create_block_table.md)
- Update variables to allow 2000 support in
  [`create_block_table()`](https://christophertkenny.com/geomander/reference/create_block_table.md)
- Add VEST import tools
- Fix edge case where no geo_match for final entries in
  [`estimate_down()`](https://christophertkenny.com/geomander/reference/estimate_down.md)
- Add ALARM import tools

## geomander 1.0.9

- Address SD and AK create_block_table assumptions

## geomander 1.0.8

CRAN release: 2021-06-16

- downgrade CRS for solaris 2005 build

## geomander 1.0.7

CRAN release: 2021-06-14

- Shortens vignettes for CRAN build speed

## geomander 1.0.6

- updates for sf 1.0-0 compatibility
- makes sure all examples run

## geomander 1.0.5

- adds orange dataset to bring back a vignette
- brings back 1/2 vignettes
- updates create_tract_table with better names
- begins use of testthat

## geomander 1.0.4

- adds rockland dataset containing simplified geometry blocks and
  precincts
- adds draft split_precinct function (functional but not 100% stable)
- introduces better race column names for create_block_table (tract
  update planned for 1.0.5)

## geomander 1.0.3

- makes vignettes GitHub only to allow for CRAN submission

## geomander 1.0.2

- makes geo_filter and geo_trim more stable and consistent
- spell checks

## geomander 1.0.1

- speed up suggest_component_connection
- adds functions for using plans with Dave’s Redistricting

## geomander 1.0.0

- First release of geomander.
