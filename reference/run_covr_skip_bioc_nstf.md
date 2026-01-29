# Run coverage with skip detection for Bioconductor RUnit-based packages

\`run_covr_skip_bioc_nstf()\` orchestrates coverage for packages using
the Bioconductor \*\*RUnit\*\* layout, such as \*\*BiocGenerics\*\* and
packages that store tests in \`inst/unitTests\` or ship a
\`tests/run_unitTests.R\` wrapper.

## Usage

``` r
run_covr_skip_bioc_nstf(pkg_source_path, test_pkg_data)
```

## Arguments

- pkg_source_path:

  Character scalar. Path to the root of the package source.

- test_pkg_data:

  A list or environment with test metadata (e.g., fields like
  \`bioc_run_ut_path\` (path to \`tests/run_unitTests.R\`, if present),
  \`has_BioG_test\` (logical flag), etc.).

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

## Examples

``` r
if (FALSE) { # \dontrun{
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

dp <- system.file("test-data", 
  "BiocGenerics_0.56.0.tar.gz", 
  package = "test.assessr")

# set up package
install_list <- set_up_pkg(dp)

package_installed <- install_list$package_installed
pkg_source_path <- install_list$pkg_source_path

# install package locally to ensure test works
package_installed <- install_package_local(pkg_source_path)
package_installed <- TRUE

test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
test_covr_list <- run_covr_skip_bioc_nstf(pkg_source_path, test_pkg_data)

options(old)
} # }
```
