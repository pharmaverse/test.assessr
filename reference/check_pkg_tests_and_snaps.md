# Check for Testing Infrastructure and Snapshot Files

This function inspects an R package source tree and detects the presence
of common testing frameworks (\`testthat\`, \`testit\`, base R tests,
BioGenerics/Bioconductor-style tests) as well as snapshot files used for
golden testing.

## Usage

``` r
check_pkg_tests_and_snaps(pkg_source_path)
```

## Arguments

- pkg_source_path:

  Character. Path to the root of the package source.

## Value

A list containing logical indicators and file counts describing the test
configuration of the package. The list includes:

- has_testthat:

  Logical. Whether \`tests/testthat/\` exists.

- has_testit:

  Logical. Whether \`tests/testit/\` exists.

- has_tests_base:

  Logical. Whether base R test files exist in \`tests/\`.

- has_BioG_test:

  Logical. Whether BioGenerics-style tests exist in \`inst/tests/\`.

- bioc_unit_tests_dir:

  Character. Path to BioGenerics unit test directory (if any).

- bioc_run_ut_path:

  Character. Path to BioGenerics \`run_unitTests.R\` (if any).

- has_snaps:

  Logical. Whether \`\_snaps/\` exists inside \`tests/testthat/\`.

- n_golden_tests:

  Integer. Number of snapshot test files inside \`\_snaps/\`.

- n_test_files:

  Integer. Number of \`test-\*.R\` files in \`tests/testthat/\`.

This function always returns a value. It does \*\*not\*\* perform side
effects other than reading the package directory structure.

## Examples

``` r
# \donttest{
# Adjust CRAN repo (example only)
r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
old <- options(repos = r)

# Example package contained in test.assessr
dp <- system.file(
  "test-data",
  "test.package.0001_0.1.0.tar.gz",
  package = "test.assessr"
)

# Set up package
install_list <- set_up_pkg(dp)
#> unpacking test.package.0001 locally
#> unpacked test.package.0001 locally
pkg_source_path <- install_list$pkg_source_path

# Install package locally (ensures correct test paths)
install_package_local(pkg_source_path)
#> installing test.package.0001 locally
#> test.package.0001 installed locally
#> [1] TRUE

# Detect tests and snapshots
test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
#> checking package test config

# Restore options
options(old)
# }
```
