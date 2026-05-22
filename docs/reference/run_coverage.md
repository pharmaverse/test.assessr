# Run Coverage and Return Structured Coverage Results

This function executes code coverage analysis for a package using the
\`covr\` framework. It is typically used after a package has been
installed locally and test files are available. The function runs
coverage in an isolated process (using \`callr\`) and returns a
structured summary of overall and file‑level coverage.

## Usage

``` r
run_coverage(pkg_source_path, timeout = Inf)
```

## Arguments

- pkg_source_path:

  Character. Path to the installed package directory from which coverage
  should be computed.

- timeout:

  Numeric. Timeout (in seconds) passed to
  [`callr::r_safe()`](https://callr.r-lib.org/reference/r.html) when
  running coverage. This limits the maximum time allowed for executing
  the tests underlying the coverage analysis.

## Value

A named list containing:

- total_cov:

  Numeric. Aggregated coverage percentage for the package.

- res_cov:

  A list containing file‑level or function‑level coverage results as
  returned by \`covr\`. This includes per‑file coverage, errors, and
  diagnostic notes if present.

Returns `NULL` if coverage could not be computed.

## Details

The function invokes covr’s coverage evaluation in a clean R session and
extracts both:

- total coverage: percentage of lines covered across the package, and

- function/file‑level coverage: coverage data for individual files.

It is used internally by higher‑level functions such as
[`run_covr_modes()`](run_covr_modes.md) and skip‑aware coverage wrappers
in the Standard Testing Framework (STF).

## Examples

``` r
# \donttest{
# Save and adjust CRAN mirror for reproducibility
r <- getOption("repos")
old <- options(repos = r)
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)

# Example package from test.assessr
dp <- system.file(
  "test-data",
  "test.package.0001_0.1.0.tar.gz",
  package = "test.assessr"
)

# Set up package source directory
install_list <- set_up_pkg(dp)
#> unpacking test.package.0001 locally
#> unpacked test.package.0001 locally
pkg_source_path <- install_list$pkg_source_path

# Install locally to enable testing and coverage
package_installed <- install_package_local(pkg_source_path)
#> installing test.package.0001 locally
#> test.package.0001 is already installed

if (isTRUE(package_installed)) {
  coverage_results <- run_coverage(pkg_source_path)
}
#> running code coverage for test.package.0001
#> code coverage for test.package.0001 successful

# Restore user's original repository settings
options(old)
# }
```
