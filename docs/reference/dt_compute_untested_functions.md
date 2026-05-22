# Compute untested functions from coverage using srcref spans (internal)

Identifies package functions that were not executed by the provided
coverage object. A function is considered *untested* if **no** line
within its `srcref` span (from `start_line` to `end_line`) has a
positive execution count in
[`covr::tally_coverage()`](http://covr.r-lib.org/reference/tally_coverage.md).

## Usage

``` r
dt_compute_untested_functions(pkg_name, coverage)
```

## Arguments

- pkg_name:

  character(1). The package name whose namespace is inspected for
  functions (`asNamespace(pkg_name)`).

- coverage:

  A coverage object as returned by `covr` (e.g.,
  [`covr::environment_coverage()`](http://covr.r-lib.org/reference/environment_coverage.md)
  or
  [`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md)),
  consumable by
  [`covr::tally_coverage()`](http://covr.r-lib.org/reference/tally_coverage.md).

## Value

[data.frame](https://rdrr.io/r/base/data.frame.html) with one row per
untested function and columns:

- function_name:

  Function symbol in the package namespace.

- file:

  Absolute, normalized path to the source file containing the function.

- start_line:

  Start line (inclusive) from the function's `srcref`.

- end_line:

  End line (inclusive) from the function's `srcref`.

Returns an empty data.frame with these columns if (a) the coverage table
is empty, (b) no functions have an `srcref`, or (c) all functions had at
least one executed line within their span.

## Details

- Functions without an `srcref` are skipped.

- Files are matched by *basename* between the coverage table and the
  function’s `srcref` source path to make the join resilient to
  differing absolute paths in temporary build trees.

- A line is considered executed when its coverage `value` is a finite
  number greater than zero.

## See also

[`tally_coverage`](http://covr.r-lib.org/reference/tally_coverage.md)
