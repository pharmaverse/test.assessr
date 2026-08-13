# Map exported functions to test files by content, not filename (internal)

Alternative to
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md) for
the `tests_base` framework: instead of matching source and test file
\*names\*, this checks which of the package's real exported functions
are actually called (by name) in each real test file's text. Requires
the package to already be loaded, since the true export list comes from
[`getNamespaceExports()`](https://rdrr.io/r/base/ns-reflect.html) - a
filename-only, load-free variant would have to fall back to hand-parsing
`NAMESPACE`, which does not reliably resolve `exportPattern()` the way a
loaded namespace does.

## Usage

``` r
get_source_test_mapping_by_exports(pkg_source_path, pkg_name, test_files)
```

## Arguments

- pkg_source_path:

  Character. Path to the package source root.

- pkg_name:

  Character. Package name; must already be a loaded namespace.

- test_files:

  Character vector. Basenames of the real test files under `tests/`
  (e.g. `base_test_files`).

## Value

A data frame with the same shape as
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md):
`source_file`, `test_file` (`NA` if no exported function defined in that
source file is called, by name, in any test file's text).

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
