# Map R Source Files to Corresponding Test Files

This internal helper function scans the \`R/\` and \`tests/\`
directories of a package and attempts to match each source file with a
corresponding test file based on filename similarity.

## Usage

``` r
get_source_test_mapping_nstf(pkg_source_path, test_dir)
```

## Arguments

- pkg_source_path:

  Character string. Path to the root of the package source directory.

- test_dir:

  character string. directory where tests are

## Value

A data frame with two columns:

- source_file:

  Name of the source file in the \`R/\` directory.

- test_file:

  Name of the matched test file in the \`tests/\` directory, or `NA` if
  no match is found.

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`prepare_tinytest_dev_includes()`](prepare_tinytest_dev_includes.md),
[`prepare_tinytest_run_env()`](prepare_tinytest_run_env.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
