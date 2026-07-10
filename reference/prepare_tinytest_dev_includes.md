# Add dev-tree include paths for self-compiling tinytest suites (internal)

Some tinytest files compile inline C/C++ during the run (e.g. via
[`Rcpp::sourceCpp()`](https://rdrr.io/pkg/Rcpp/man/sourceCpp.html)).
When coverage loads the package from its source tree with
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html),
public headers live under `inst/include/`, but the compiler flags
emitted by self-compilation helpers typically reference the *installed*
layout (`include/`). Packages that ship headers in `inst/include` (e.g.
Rcpp) therefore fail with ‘Rcpp.h: No such file or directory’ unless the
source-tree include directory is prepended to the preprocessor flags.

## Usage

``` r
prepare_tinytest_dev_includes(pkg_source_path, tiny_dir)
```

## Arguments

- pkg_source_path:

  Character scalar. Path to the package source root.

- tiny_dir:

  Character scalar. Path to the tinytest directory (typically
  `inst/tinytest`).

## Value

A list with components:

- include_dirs:

  Character vector of absolute include directories that were added;
  empty when no dev-tree headers are present.

- teardown:

  A function that restores the previous `PKG_CPPFLAGS` and
  `CLINK_CPPFLAGS` values (unsetting variables that were previously
  unset).

## Details

This helper detects `inst/include/` under `pkg_source_path`, optionally
adds `<tiny_dir>/cpp/` when present, and prepends corresponding `-I`
flags to `PKG_CPPFLAGS` and `CLINK_CPPFLAGS`. Packages without
`inst/include/` (e.g. digest) are unchanged.

The function is intentionally defensive and never throws: on any problem
it returns a no-op result so the surrounding tinytest coverage path is
unaffected. Callers should register the returned `teardown` via
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) to restore the
previous environment state.

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`prepare_tinytest_run_env()`](prepare_tinytest_run_env.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
