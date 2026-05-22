# Run Coverage Analysis on data.table Package Source and Tests

This function maps source files to test files, checks for
skipped/problematic tests, runs the remaining test files manually,
calculates coverage, and optionally generates a report.

## Usage

``` r
run_covr_skip_dt_nstf(pkg_source_path, test_pkg_data, cov_env)
```

## Arguments

- pkg_source_path:

  Character string. Path to the root of the package source directory.

- test_pkg_data:

  List. Output from \`check_pkg_tests_and_snaps()\` indicating test
  framework presence.

- cov_env:

  Environment that holds the temporary coverage workspace and shared
  state for the run.

## Value

returns a list with coverage object and source files with no tests.
