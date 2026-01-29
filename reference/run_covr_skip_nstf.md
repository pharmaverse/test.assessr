# Run Coverage Analysis on Package Source and Tests

This function maps source files to test files, checks for
skipped/problematic tests, runs the remaining test files manually,
calculates coverage, and optionally generates a report.

## Usage

``` r
run_covr_skip_nstf(pkg_source_path, test_pkg_data, preserve_vars = "old")
```

## Arguments

- pkg_source_path:

  Character string. Path to the root of the package source directory.

- test_pkg_data:

  List. Output from \`check_pkg_tests_and_snaps()\` indicating test
  framework presence.

- preserve_vars:

  Character. preserve \`old\` for CRAN examples

## Value

returns a list with coverage object and source files with no tests.

## Examples

``` r
if (FALSE) { # \dontrun{
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
old <- options(repos = r)

dp <- system.file("test-data", 
  "MASS_7.3-65.tar.gz", 
  package = "test.assessr")

# set up package
install_list <- set_up_pkg(dp)

package_installed <- install_list$package_installed
pkg_source_path <- install_list$pkg_source_path

# install package locally to ensure test works
package_installed <- install_package_local(pkg_source_path)
package_installed <- TRUE

test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
test_covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)

options(old)
} # }
```
