# Run Coverage Analysis with Test Detection

This function inspects the test configuration of an R package and runs
code coverage analysis using any available testing framework, including
\`testthat\`, \`testit\`, base R test scripts, or Bioconductor-style
tests. If no recognised testing configuration is found, a default
zero‑coverage result is returned.

## Usage

``` r
run_covr_modes(pkg_source_path, covr_timeout = 60)
```

## Arguments

- pkg_source_path:

  Character. Path to the root directory of the package source.

- covr_timeout:

  Numeric. Timeout in seconds for running coverage analysis. Default is
  60.

## Value

A named list containing coverage results, package metadata, and test
configuration details. The returned list includes (but is not limited
to):

- pkg_name:

  Character. Package name extracted from the DESCRIPTION file.

- pkg_ver:

  Character. Package version.

- date_time:

  Timestamp of when the analysis was run.

- executor:

  User or environment running the analysis.

- sysname, version, release, machine:

  System metadata.

- r_version:

  R version used during analysis.

- test_framework_type:

  Character. Detected testing framework type.

- covr_list:

  A nested list containing:

  total_cov

  :   Numeric. Aggregated coverage percentage.

  res_cov

  :   File‑level and line‑level coverage details.

  errors, notes

  :   Any warnings or notes detected during testing.

- test_pkg_data:

  A list describing the test configuration of the package (presence of
  testthat, testit, base tests, snapshots, etc.).

This function always returns a value. When no supported testing
framework is detected, a default object with zero coverage and
diagnostic information is returned.

## Examples

``` r
# \donttest{

dp <- system.file("test-data", 
  "test.package.0001_0.1.0.tar.gz", 
  package = "test.assessr")

# set up package
install_list <- set_up_pkg(dp)
#> unpacking test.package.0001 locally
#> unpacked test.package.0001 locally

package_installed <- install_list$package_installed
pkg_source_path <- install_list$pkg_source_path

# install package locally to ensure test works
package_installed <- install_package_local(pkg_source_path)
#> installing test.package.0001 locally
#> test.package.0001 is already installed
package_installed <- TRUE

covr_mode_list <- run_covr_modes(pkg_source_path)
#> checking package test config
#> Unloading test.package.0001 package to avoid coverage conflict...
#> decider: testthat -> run_coverage()
#> running code coverage for test.package.0001
#> code coverage for test.package.0001 successful
#> Cleaning coverage workspace for test.package.0001

# }
```
