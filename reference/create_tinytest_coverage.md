# Create coverage from tinytest under covr instrumentation (internal)

Instruments the package \*\*namespace\*\* with covr, runs all tinytest
files in `inst/tinytest`, and returns a covr coverage object together
with the tinytest results. This mirrors tinytest's execution model
(expectation helpers available; tests run from each file's directory)
while ensuring calls into the package namespace are traced.

## Usage

``` r
create_tinytest_coverage(
  pkg_source_path,
  pkg_name,
  tiny_dir = file.path(pkg_source_path, "inst", "tinytest"),
  at_home = TRUE,
  color = FALSE
)
```

## Arguments

- pkg_source_path:

  Character scalar. Path to the package source root.

- pkg_name:

  Character scalar. Package name (as in `DESCRIPTION`).

- tiny_dir:

  Optional override for the tinytest directory. Defaults to
  `file.path(pkg_source_path, "inst", "tinytest")`.

- at_home:

  Logical; passed to
  [`tinytest::run_test_dir()`](https://rdrr.io/pkg/tinytest/man/run_test_dir.html).
  Defaults to `TRUE`.

- color:

  Logical; color output from tinytest. Defaults to `FALSE`.

## Value

A list with components:

- coverage:

  An object of class `"coverage"` (from covr).

- tinytests:

  The `tinytests` object returned by tinytest.

## Details

Intended for \*\*source-tree\*\* coverage of tinytest suites:

- The package namespace is traced via `covr::trace_environment()` so
  that calls made by tests to package functions are recorded for
  coverage.

- `NOT_CRAN` is set to `"true"` during the run (restored on exit) so
  tinytest `at_home()` logic treats the run as local.

- Environment-gated suites (e.g. Rcpp's `RunAllRcppTests`) are enabled
  via [`prepare_tinytest_run_env()`](prepare_tinytest_run_env.md) before
  the run and restored on exit, so gated tests execute instead of
  exiting early.

- Packages with headers in `inst/include/` that self-compile during
  tests receive dev-tree `-I` flags via
  [`prepare_tinytest_dev_includes()`](prepare_tinytest_dev_includes.md)
  (restored on exit).

- Test files are driven by
  [`tinytest::run_test_dir()`](https://rdrr.io/pkg/tinytest/man/run_test_dir.html)
  from `file.path(pkg_source_path, "inst", "tinytest")`. A fatal
  top-level error in a single file (e.g. a test that asserts the
  installed package layout, which differs from a `load_all` source tree)
  is caught so it does not discard coverage already recorded for the
  files that ran.

- Coverage is built from covr's counters using
  [`covr::as_coverage()`](http://covr.r-lib.org/reference/as_coverage.md).

- Objects that test files create in `.GlobalEnv` are removed on exit.
  Some suites (e.g. Rcpp) call
  [`Rcpp::sourceCpp()`](https://rdrr.io/pkg/Rcpp/man/sourceCpp.html),
  whose `env` argument defaults to
  [`globalenv()`](https://rdrr.io/r/base/environment.html), exporting
  many compiled wrappers into the user's workspace. `.GlobalEnv` is
  snapshotted before the run and only newly-added objects are removed
  afterwards, so pre-existing objects are preserved.

**Preconditions**

- The package under test should already be *loaded* (e.g.,
  `pkgload::load_all(pkg_source_path)`), so that `asNamespace(pkg_name)`
  exists and reflects the current source.

- The directory `inst/tinytest` must exist and contain test files.

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`prepare_tinytest_dev_includes()`](prepare_tinytest_dev_includes.md),
[`prepare_tinytest_run_env()`](prepare_tinytest_run_env.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
