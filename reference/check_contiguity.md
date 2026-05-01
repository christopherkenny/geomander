# Check Contiguity by Group

Identify contiguous sets of units and numbers each set. Can be extended
to repeat the procedure within a subgeography.

## Usage

``` r
check_contiguity(adj, group)

cct(adj, group)

ccm(adj, group)
```

## Arguments

- adj:

  Zero-indexed adjacency list.

- group:

  Optional vector of group identifiers, typically district numbers or
  county names. If omitted, all rows are treated as belonging to one
  group.

## Value

tibble with contiguity indicators. Each row is the units of `adj`.
Columns include

- `group` Values of the inputted `group` argument. If `group` is not
  specified, then all values will be 1.

- `group_number` Integer encoding of `group`.

- `component` A number for each contiguous set of units within a
  `group`. If all units within a `group` are contiguous, all values
  are 1. If there are two sets, each discontiguous with the other, the
  larger one will be numbered 1 and the smaller one will be numbered 2.

## Details

Given a zero-indexed adjacency list and an array of group identifiers,
this returns a tibble which identifies the connected components. The
three columns are `group` for the inputted group, `group_number` which
uniquely identifies each group as a positive integer, and `component`
which identifies the connected component number for each corresponding
entry of adjacency and group. If everything is connected within the
group, then each element of `component` will be `1`. Otherwise, the
largest component is given the value `1`, the next largest `2`, and so
on.

If nothing is provided to group, it will default to a vector of ones,
checking if the adjacency graph is connected.

`cct()` is shorthand for creating a table of the component values. If
everything is connected within each group, it returns a value of 1. In
general, it returns a frequency table of components.

`ccm()` is shorthand for getting the maximum component value. It returns
the maximum number of components that a group is broken into. This
returns 1 if each group is connected. \#'

## Examples

``` r
data(checkerboard)
adj <- adjacency(checkerboard)
#> Warning: Planarizing skipped. `x` missing CRS.
# These each indicate the graph is connected.
check_contiguity(adj) # all contiguous
#> # A tibble: 64 × 3
#>    group group_number component
#>    <int>        <int>     <int>
#>  1     1            1         1
#>  2     1            1         1
#>  3     1            1         1
#>  4     1            1         1
#>  5     1            1         1
#>  6     1            1         1
#>  7     1            1         1
#>  8     1            1         1
#>  9     1            1         1
#> 10     1            1         1
#> # ℹ 54 more rows
# If there are two discontiguous groups, there will be 2 values of `component`
cct(adj)
#> 
#>  1 
#> 64 
ccm(adj)
#> [1] 1
```
