# Create coverage by running base-style tests under covr (internal)

Traces the package namespace with covr, then sources each test script
from its own directory (so relative file references like
'load("solr.rda")' in MASS/tests work). Converts covr counters to a
coverage object.

## Usage

``` r
create_base_tests_coverage(pkg_source_path, pkg_name, test_files)
```

## Arguments

- pkg_source_path:

  Character; package source root.

- pkg_name:

  Character; package name.

- test_files:

  Character vector of test file paths (absolute or relative).

## Value

list(coverage = \<covr coverage object\>)

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
