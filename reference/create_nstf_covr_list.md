# Create the NSTF Coverage List

Internal helper that constructs the final \`covr_list\` structure from a
coverage object. It wraps extraction of coverage information, performs
total coverage computation, and assembles all diagnostic components.

## Usage

``` r
create_nstf_covr_list(
  coverage,
  pkg_name,
  no_tests_df,
  test_files_clean,
  skip_tests,
  problems
)
```

## Arguments

- coverage:

  A coverage object returned from covr.

- pkg_name:

  A Character string - package name.

- no_tests_df:

  A dataframe - functions with no tests.

- test_files_clean:

  A Character vector - functions with working tests

- skip_tests:

  A Character vector - functions skipped

- problems:

  A Character vector - test problems

## Value

A named list (\`covr_list\`) containing:

- total_cov:

  Numeric; overall package coverage percentage.

- res_cov:

  List; raw coverage extraction results with coverage data, errors,
  notes.

- functions_no_tests:

  Data frame of functions lacking tests.

- tests_skipped:

  Character vector of skipped test files.

- tests_passing:

  Character vector of passing test files.

- tests_problems:

  Character vector describing test problems.

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`prepare_tinytest_dev_includes()`](prepare_tinytest_dev_includes.md),
[`prepare_tinytest_run_env()`](prepare_tinytest_run_env.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
