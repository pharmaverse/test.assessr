# Run coverage for a single test framework

Run coverage for a single test framework

## Usage

``` r
run_one_framework(framework, pkg, test_pkg_data, pkg_source_path, covr_timeout)
```

## Arguments

- framework:

  Character. Framework ID.

- pkg:

  Character. Package name.

- test_pkg_data:

  List from `check_pkg_tests_and_snaps`.

- pkg_source_path:

  Character. Path to package source.

- covr_timeout:

  Numeric. Timeout for coverage.

## Value

List with total_cov, res_cov, test_framework_type, framework_id.
