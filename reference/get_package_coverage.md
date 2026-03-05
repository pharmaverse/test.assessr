# get package test coverage

simplified input to assess package for test coverage

## Usage

``` r
get_package_coverage(path = NULL)
```

## Arguments

- path:

  (optional) path of locally stored package source code

## Value

An object of class `"coverage"` as produced by
[`test.assessr::run_covr_modes()`](run_covr_modes.md). This object is a
structured list containing detailed test coverage information,
including:

- **File-level coverage:** percentage of lines covered in each file.

- **Line-by-line coverage:** number of executions for each line.

- **Overall coverage:** aggregated coverage across the package.

- **Metadata:** source file paths, expressions, and summary stats.

The resulting object can be printed, summarized, or passed to
[`test.assessr::generate_test_report()`](generate_test_report.md) to
produce a human-readable test coverage report.

Returns `NULL` if the package cannot be installed or if the specified
path does not exist.

## Examples

``` r
# \donttest{
# Example STF package included in test.assessr
pkg_source_path <- system.file(
  "test-data",
  "test.package.0001_0.1.0.tar.gz",
  package = "test.assessr"
)

# Run get_package_coverage
get_package_coverage <- get_package_coverage(pkg_source_path)
#> unpacking test.package.0001 locally
#> unpacked test.package.0001 locally
#> installing test.package.0001 locally
#> test.package.0001 is already installed
#> Unloading test.package.0001 package to avoid coverage conflict...
#> checking package test config
#> running standard testing framework
#> running code coverage for test.package.0001
#> code coverage for test.package.0001 successful
#> Cleaning coverage workspace for test.package.0001
# }
```
