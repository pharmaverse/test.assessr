# Run testit tests individually, capturing all warnings and errors, with dynamic package root detection

Run testit tests individually, capturing all warnings and errors, with
dynamic package root detection

## Usage

``` r
check_covr_skip_testit(pkg_name, mapping, test_path)
```

## Arguments

- pkg_name:

  The package name (not used in this function, but kept for
  compatibility)

- mapping:

  A data frame with a column \`test_file\` listing test file names

- test_path:

  The path to the test directory (e.g., tests/testit)

## Value

A data frame with test_file and issue_type columns

## See also

Other nstf_utility:
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`prepare_tinytest_dev_includes()`](prepare_tinytest_dev_includes.md),
[`prepare_tinytest_run_env()`](prepare_tinytest_run_env.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
