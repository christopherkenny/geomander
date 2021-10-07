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