# Run Coverage with Skipped Failing Tests (Standard Testing Framework)

This function runs testthat-based test execution and code coverage while
identifying and skipping failing tests. It is designed for use with
standard testing frameworks (STF), primarily \`testthat\`, and supports
automated skip mapping to enable coverage calculation even when some
tests fail.

## Usage

``` r
run_covr_skip_stf(pkg_source_path, test_pkg_data, cov_env)
```

## Arguments

- pkg_source_path:

  Character. Path to the root directory of the package source.

- test_pkg_data:

  List. Output from
  [`check_pkg_tests_and_snaps()`](check_pkg_tests_and_snaps.md),
  containing information about the package's testing setup (presence of
  testthat, snapshots, etc.).

- cov_env:

  Environment that holds the temporary coverage workspace and shared
  state for the run. This must be an environment created by
  \`initialise_covr_env()\`, which sets \`cov_env\$work_dir\` to a
  unique subdirectory under \`tempdir()\`. \`run_covr_skip_stf()\` will
  not create or modify \`cov_env\` beyond reading helpers into it; all
  file outputs produced by downstream helpers are expected to be written
  under \`cov_env\$work_dir\` (never to the package directory or the
  user's home). The caller is responsible for lifecycle management;
  register \`on.exit(cleanup_and_return_null(env = cov_env), add =
  TRUE)\` in the calling scope to ensure bindings and the temporary
  workspace are cleaned up even on error.

## Value

A named list containing coverage results and test diagnostics. The
structure includes:

- total_cov:

  Numeric. Overall coverage percentage.

- res_cov:

  A nested list with file-level coverage, errors, and notes.

- long_summary:

  Long-format test results including PASS/FAIL/ERROR/SKIPPED.

- test_skip:

  A data frame describing test lines skipped during coverage, if
  applicable.

If no tests fail, the returned list describes full coverage based on
unskipped tests. If no valid STF test directory is found, an empty
coverage object is returned via
[`create_empty_covr_list()`](create_empty_covr_list.md).

## Details

The function:

- executes tests and collects detailed test results,

- identifies failing, erroring, or skipped tests,

- constructs skip maps to temporarily comment out failing test lines,

- computes coverage on the remaining tests, and

- returns a combined object containing coverage and test summaries.

## See also

Other stf_utility:
[`create_coverage_skip_stf()`](create_coverage_skip_stf.md),
[`map_test()`](map_test.md), [`map_tests_stf()`](map_tests_stf.md),
[`nest_test()`](nest_test.md)
