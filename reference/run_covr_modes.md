# Run Coverage Analysis with Test Detection

This function checks the test configuration of an R package and runs
code coverage analysis using \`testthat\`, \`testit\` or non-standard
testing frameworks if available. If no supported test framework is
found, it returns a default coverage result indicating no functions were
tested.

## Usage

``` r
run_covr_modes(pkg_source_path, covr_timeout = 60, preserve_vars = "old")
```

## Arguments

- pkg_source_path:

  Character. Path to the root directory of the R package source.

- covr_timeout:

  Numeric. Timeout in seconds for running coverage analysis. Default is
  60.

- preserve_vars:

  Character. preserve \`old\` for CRAN examples

## Value

A list with two elements:

- covr_list:

  A list containing coverage results. If no supported tests are found,
  returns a default structure with zero coverage.

- test_pkg_data:

  A list summarizing the test configuration, including presence of
  \`testthat\`, \`testit\`, snapshots, and base R test scripts.

## Examples

``` r
if (FALSE) { # \dontrun{
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
old <- options(repos = r)

dp <- system.file("test-data", 
  "test.package.0001_0.1.0.tar.gz", 
  package = "test.assessr")

# set up package
install_list <- set_up_pkg(dp)

package_installed <- install_list$package_installed
pkg_source_path <- install_list$pkg_source_path

# install package locally to ensure test works
package_installed <- install_package_local(pkg_source_path)
package_installed <- TRUE

covr_mode_list <- run_covr_modes(pkg_source_path)

options(old)
} # }
```
