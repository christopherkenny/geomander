# geomander 2.3.0

* Adds support for downloading Jeffrey B. Lewis's historical congressional districts with `get_lewis()
* Adds support for downloading RPV Near Me datasets with `get_rpvnearme()`
* Corrects normalized global Moran's I calculation (#12), thanks @CoryMcCartan.
* Fixes a download issue for `get_dra()`

# geomander 2.2.0
* Updates dependencies for easier installation.
* Allows for use of `circle` as a method in `geo_match()`.
* Adds a `by` argument to `geo_match()` for subsetting to known matching regions.
* Uses `redist` in vignettes only conditionally.
* Massive improvements in performance for `check_contiguity()`.

# geomander 2.1.1
* Fixes missing tolerance bug in `geos_circle_center()`.
* Adds experimental regionalizion approach, `regionalize()`.

# geomander 2.1.0
* Avoid indirect download for `get_alarm()`. Fixes bug where block level data resulted in mismatched geometry.
* Add `st_circle_center()` and `geos_circle_center()` for centroid of the maximum inscribed circle.
* Add `baf_to_vtd()` function to approximate BAFs at the VTD level.

# geomander 2.0.2
* Drop `spdep` dependency to avoid timeouts on checking examples.

# geomander 2.0.0
* `check_contiguity()` orders by most frequent component, so less common components have higher numbers.
* Uses `geos` for geographic computations over `sf` to avoid `s2` where possible and introduce speed improvements.
* Uses `cli` package for progress reporting.
* `adjacency` renamed to `adj` to match `redist` naming.
* Updates `r2dra` and `dra2r` to work off of 2020 geometries.
* Majority of functions gain `epsg` argument to project for accuracy relative to projection.

# geomander 1.1.2
* Fixes website redirects for CRAN.
* Improves flexibility in `block2prec`.
* Updates default year to 2020 for `create_block_table()`.
* Adds a polygon contiguity check. Slower than the standard contiguity check, but covers more edge cases.

# geomander 1.1.1
* Replace `PL94171` import with `tigris`.

# geomander 1.1.0
* Collapse county calls into one for `create_block_table()`
* Update variables to allow 2000 support in `create_block_table()`
* Add VEST import tools
* Fix edge case where no geo_match for final entries in `estimate_down()`
* Add ALARM import tools

# geomander 1.0.9
* Address SD and AK create_block_table assumptions

# geomander 1.0.8
* downgrade CRS for solaris 2005 build

# geomander 1.0.7 
* Shortens vignettes for CRAN build speed

# geomander 1.0.6
* updates for sf 1.0-0 compatibility
* makes sure all examples run

# geomander 1.0.5
* adds orange dataset to bring back a vignette
* brings back 1/2 vignettes
* updates create_tract_table with better names
* begins use of testthat

# geomander 1.0.4
* adds rockland dataset containing simplified geometry blocks and precincts
* adds draft split_precinct function (functional but not 100% stable)
* introduces better race column names for create_block_table (tract update planned for 1.0.5)

# geomander 1.0.3
* makes vignettes GitHub only to allow for CRAN submission

# geomander 1.0.2
* makes geo_filter and geo_trim more stable and consistent
* spell checks

# geomander 1.0.1
* speed up suggest_component_connection
* adds functions for using plans with Dave's Redistricting

# geomander 1.0.0

* First release of geomander.