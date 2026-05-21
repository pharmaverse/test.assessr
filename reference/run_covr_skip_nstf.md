# Run Coverage Analysis on Package Source and Non‑Standard Tests (NSTF)

This function maps package source files to non‑standard test scripts,
detects failing or skipped tests, executes the remaining tests manually,
computes code coverage, and returns structured results. It is intended
for packages that do not use the standard \`testthat\` framework (e.g.,
base R tests under \`tests/\`, Bioconductor‑style tests in
\`inst/tests/\`, or other legacy/custom scripts).

## Usage

``` r
run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env, framework = NULL)
```

## Arguments

- pkg_source_path:

  Character. Path to the root of the package source directory.

- test_pkg_data:

  List. Output from
  [`check_pkg_tests_and_snaps()`](check_pkg_tests_and_snaps.md)
  indicating the detected testing infrastructure (e.g., base tests,
  Bioconductor tests) and the presence of snapshots or related
  artifacts.

- cov_env:

  Environment that holds the temporary coverage workspace and shared
  state for the run.

- framework:

  Optional character. When provided (e.g. from `run_one_framework`),
  restricts test paths to the framework-specific directory. Use when a
  package has multiple frameworks (e.g. testthat + testit) to avoid
  mixing paths across frameworks.

## Value

A named list containing coverage results and diagnostics:

- total_cov:

  Numeric. Overall percentage of lines covered across the package.

- res_cov:

  List. File‑ or element‑level coverage results (as derived from covr
  output), including per‑file coverage, any errors, and diagnostic
  notes.

- long_summary:

  Long‑format summary of test outcomes (e.g., PASS/FAIL/ERROR/SKIPPED),
  when available from the NSTF execution.

- test_skip:

  A data frame describing lines or regions skipped during coverage (if
  any). `NULL` if no skipping was required.

- untested_sources:

  Character vector of source files for which no tests were found.

If no valid NSTF test directory is detected, an “empty” coverage object
may be returned via
[`create_empty_covr_list()`](create_empty_covr_list.md).

## Details

This NSTF workflow does not rely on \`testthat\` reporters. Instead, it:

- maps source files to available NSTF test scripts,

- identifies failing or problematic tests and derives a skip map,

- runs the remaining test scripts, and

- computes coverage on the successfully executed regions.

The output mirrors the structure returned by the STF skip‑aware workflow
so that downstream reporting remains consistent across frameworks.

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md)
