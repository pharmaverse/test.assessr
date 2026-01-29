# Run covr with failing tests \#' This function runs test coverage and skip analysis for a package using testthat or other supported frameworks.

Run covr with failing tests \#' This function runs test coverage and
skip analysis for a package using testthat or other supported
frameworks.

## Usage

``` r
run_covr_skip_stf(pkg_source_path, test_pkg_data)
```

## Arguments

- pkg_source_path:

  Character. Path to the root of the package source.

- test_pkg_data:

  List. Output from \`check_pkg_tests_and_snaps()\` indicating test
  framework presence.

## Value

skip_results - test coverage with PASS tests passing or FAIL for tests
failing

## Examples

``` r
if (FALSE) { # \dontrun{
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
old <- options(repos = r)

dp <- system.file("test-data", 
  "test.package.0014_0.1.0.tar.gz", 
  package = "test.assessr")

# set up package
install_list <- set_up_pkg(dp)

build_vignettes <- install_list$build_vignettes
package_installed <- install_list$package_installed
pkg_source_path <- install_list$pkg_source_path
rcmdcheck_args <- install_list$rcmdcheck_args

# install package locally to ensure test works
package_installed <- install_package_local(pkg_source_path)
package_installed <- TRUE

rcmdcheck_args$path <- pkg_source_path
test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
test_covr_list <- run_covr_skip_stf(pkg_source_path, test_pkg_data)

options(old)
} # }  
```
