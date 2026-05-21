# Detect which test frameworks are present in a package

Detect which test frameworks are present in a package

## Usage

``` r
detect_frameworks(test_pkg_data)
```

## Arguments

- test_pkg_data:

  List from `check_pkg_tests_and_snaps`.

## Value

Character vector of framework IDs: "testthat", "testit", "tests_base",
"tinytest", "data.table", "bioconductor".
