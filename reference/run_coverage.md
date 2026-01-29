# Run covr and potentially save results to disk

Run covr and potentially save results to disk

## Usage

``` r
run_coverage(pkg_source_path, timeout = Inf)
```

## Arguments

- pkg_source_path:

  package installation directory

- timeout:

  Timeout to pass to \[callr::r_safe()\] when running covr.

## Value

list with total coverage and function coverage

## Examples

``` r
# \donttest{
r <- getOption("repos")
# save current repo options
old <- options(repos = r)
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

dp <- system.file("test-data", "test.package.0001_0.1.0.tar.gz",
                  package = "test.assessr")

install_list <- set_up_pkg(dp)
#> unpacking test.package.0001 locally
#> unpacked test.package.0001 locally

build_vignettes <- install_list$build_vignettes
package_installed <- install_list$package_installed
pkg_source_path <- install_list$pkg_source_path
rcmdcheck_args <- install_list$rcmdcheck_args

package_installed <- install_package_local(pkg_source_path)
#> installing test.package.0001 locally
#> test.package.0001 is already installed
package_installed <- TRUE

if (package_installed == TRUE) {
   test_package <- run_coverage(pkg_source_path)
} else {
  message("Package installation failed.")
}
#> running code coverage for test.package.0001
#> code coverage for test.package.0001 successful
# restore user's repo options
options(old)
# }
```
