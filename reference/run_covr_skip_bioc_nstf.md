# Run coverage with skip detection for Bioconductor RUnit-based packages

\`run_covr_skip_bioc_nstf()\` orchestrates coverage for packages using
the Bioconductor \*\*RUnit\*\* layout, such as \*\*BiocGenerics\*\* and
packages that store tests in \`inst/unitTests\` or ship a
\`tests/run_unitTests.R\` wrapper.

## Usage

``` r
run_covr_skip_bioc_nstf(pkg_source_path, test_pkg_data, cov_env)
```

## Arguments

- pkg_source_path:

  Character scalar. Path to the root of the package source.

- test_pkg_data:

  A list or environment with test metadata (e.g., fields like
  \`bioc_run_ut_path\` (path to \`tests/run_unitTests.R\`, if present),
  \`has_BioG_test\` (logical flag), etc.).

- cov_env:

  Environment that holds the temporary coverage workspace and shared
  state for the run.

## Value

A \`covr\` coverage object on success; \`NULL\` on failure (via
\`cleanup_and_return_null()\`).

A covr coverage object, or NULL on failure.

## Details

The function: 1. Builds a source-to-test mapping (using your existing
\`get_source_test_mapping_nstf()\`). 2. Removes any \`run_unitTests.R\`
wrapper from the mapping (via \`remove_run_unitTests_mapping()\`). 3.
Prepares a development environment \*\*before\*\* running RUnit tests: -
Detaches commonly attached BioC infrastructure packages (\`IRanges\`,
\`S4Vectors\`, \`BiocGenerics\`) to avoid namespace/shim conflicts. -
Calls \`pkgload::load_all()\` to load the package under test. - Clears
\`R_TESTS\` to avoid R CMD check harness interference when running RUnit
programmatically. 4. Identifies problem/skip-able tests via your
RUnit-aware \`check_covr_skip_nstf()\` (which should use
\`RUnit::runTestFile()\` + \`RUnit::getErrors()\` per file). 5. Builds a
list of test files \*\*under \`inst/unitTests\`\*\* excluding any that
should be skipped. 6. Exposes namespace objects + datasets to a
dedicated environment and runs coverage with
\`covr::environment_coverage()\`.

Use this function \*\*only\*\* for packages that follow the Bioconductor
RUnit layout: - tests are located in \`inst/unitTests/\`, and/or - a
\`tests/run_unitTests.R\` wrapper exists that calls an internal
\`.test()\` / \`testPackage()\` runner.
